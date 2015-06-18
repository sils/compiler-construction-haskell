module CodeGenerator where

import AbsCPP
import PrintCPP
import Control.Monad
import Control.Monad.State

-- Type of LLVMExpressions mostly Name of a temporary
type LLVMExpr = String
-- compiled LLVM Instruction
type Instruction = String
-- Type for llvm types
type LLVMType = String
-- Variable information: Name and typ
data VarInfo = VI {
  mangled :: LLVMExpr,
  typ :: LLVMType
}
-- Function information: Name and return type
data FunInfo = FI {
  mangledF :: LLVMExpr,
  retTyp :: LLVMType
}

getMangled :: VarInfo -> LLVMExpr
getMangled varinfo = ("%"++(mangled varinfo))

-------------------------------------------------------------------------------------------------
--Environment methods
-------------------------------------------------------------------------------------------------

-- Environment, nextTemp identifier, code, vars and functions
data Env = E {
  nextTemp :: LLVMExpr,
  nextLabel :: LLVMExpr,
  mangledIds :: [LLVMExpr],
  code :: [Instruction],
  vars :: [[(Id, VarInfo)]],
  funs :: [[(Id, FunInfo)]]
}
initEnv :: Env
initEnv = E {
  nextTemp = "1",
  nextLabel = "1",
  mangledIds = [],
  code = [],
  vars = [[]],
  funs = [[]]
}

-- get Identifier of next temporary variable, updates nextTemp in environment
getNextTemp :: State Env LLVMExpr
getNextTemp = do
  env <- get
  let tmp = nextTemp env
  modify (\env -> env {nextTemp = show (read (nextTemp env) + 1)})
  return ("%"++tmp)

getNextLabel :: State Env LLVMExpr
getNextLabel = do
  env <- get
  let tmp = nextLabel env
  modify (\env -> env {nextLabel = show (read (nextLabel env) + 1)})
  return ("l"++tmp)

-- reset next temporary variable counter
resetNextTemp :: State Env ()
resetNextTemp = modify (\env -> env{nextTemp = "1"})

-- Enter new scope
enterScope :: State Env ()
enterScope = modify (\env -> env {vars = [] : vars env})

-- Exit outermost scope
exitScope :: State Env ()
exitScope = modify (\env -> env {
  vars = case vars env of {
    (scope:rest) -> rest;
    _            -> error $ "Can't exit non existent scope"
  }
})

-- Add Variable to Environment
addVar :: Id -> Type -> State Env VarInfo
addVar id typ =
  do
    mangledName <- mangleName id
    let varinfo = VI mangledName (getLLVMType typ)
    modify (\env -> env {
    vars = case vars env of
      (scope:rest) -> ((id, varinfo) : scope) : rest;
      _            -> [[(id, varinfo)]]
    })
    return varinfo

-- Lookup given variable and return it's info
lookupVar :: Id -> State Env VarInfo
lookupVar id = do
    env <- get
    return $ look (vars env) id
  where
    look [] id = error $ "Unknown variable " ++ printTree id ++ "."
    look (scope:rest) id = case lookup id scope of
      Nothing -> look rest id
      Just info -> info

-- Add Function to Environment
addFun :: Id -> Type -> State Env ()
addFun id typ =
  do
    mangledName <- mangleName id
    let funInfo = FI mangledName (getLLVMType typ)
    modify (\env -> env {
    funs = case funs env of
      (scope:rest) -> ((id, funInfo) : scope) : rest;
      _            -> [[(id, funInfo)]]
    })
    return ()

-- Lookup given variable and return it's info
lookupFun :: Id -> State Env FunInfo
lookupFun id = do
    env <- get
    return $ look (funs env) id
  where
    look [] id = error $ "Unknown function " ++ printTree id ++ "."
    look (scope:rest) id = case lookup id scope of
      Nothing -> look rest id
      Just info -> info

-- emits instruction to environment
emit :: Instruction -> State Env ()
emit i = modify (\env -> env {code = code env ++ [i]})

-------------------------------------------------------------------------------------------------
--CodeGenerator methods
-------------------------------------------------------------------------------------------------

-- Generate code for Program
codeGen :: Program -> [Instruction]
codeGen (PDefs defs) = code $ execState (generate defs) initEnv

generate :: [Def] -> State Env ()
generate defs =
  do
    checkDecls defs
    codeGenDefs defs

-- Check a list of declarations
checkDecls :: [Def] -> State Env ()
checkDecls defs = mapM_ checkDecl defs

-- Check a declaration for validity
checkDecl :: Def -> State Env ()
checkDecl def =
  case def of
    DFun typ identifier args stmts -> do
      addFun identifier typ

-- Generate code for Definitions
codeGenDefs :: [Def] -> State Env ()
codeGenDefs defs = mapM_ codeGenDef defs

-- Generate code for Definition
codeGenDef :: Def -> State Env ()
codeGenDef (DFun typ id args stmts) = do
  enterScope
  -- add arguments to current sope
  mapM_ (\(ADecl typ id) -> addVar id typ) args
  infos <- mapM (\(ADecl _ id) -> lookupVar id) args
  emit (define typ id infos)
  mapM_ codeGenArg args
  codeGenStmts stmts typ
  if (getLLVMType typ) == "double" then
    emit ("ret " ++ (getLLVMType typ) ++ " 0.0")
  else
    emit ("ret " ++ (getLLVMType typ) ++ " 0")
  emit "}\n"
  exitScope
  resetNextTemp
  return ()

-- Generate code for function argument
codeGenArg :: Arg -> State Env ()
codeGenArg (ADecl _ id) = do
  tmp <- getNextTemp
  info <- lookupVar id
  emit (allocate (typ info) tmp)
  emit (store (typ info) (getMangled info) tmp)
  modify (\env -> env {vars = case vars env of
    (scope:rest) -> ((id, VI (tail tmp) (typ info)) : scope) : rest
    _            -> [[(id, VI (tail tmp) (typ info))]]
  })
  return ()

-- Generate code for Statements
codeGenStmts :: [Stm] -> Type -> State Env ()
codeGenStmts stmts typ = mapM_ (\stmt -> (codeGenStmt stmt typ)) stmts

-- Generate code for Statement
codeGenStmt :: Stm -> Type -> State Env ()
codeGenStmt stm rettyp =
  case stm of
    SExp exp                 ->
      do
        codeGenExpr exp
        return ()
    SDecls typ identifiers   ->
      do
        varInfos <- mapM (\id -> addVar id typ) identifiers
        mapM_ (\varInfo -> emit (allocate (getLLVMType typ) (getMangled varInfo))) varInfos
    SInit typ identifier exp ->
      do
        varinfo <- addVar identifier typ
        emit (allocate (getLLVMType typ) (getMangled varinfo))
        (tmp,_) <- codeGenExpr exp
        emit (store (getLLVMType typ) tmp (getMangled varinfo))
    SReturn exp              ->
      do
        (tmp,_) <- codeGenExpr exp
        -- increment nextTemp counter cause return ends a basic block
        getNextTemp
        emit ("ret " ++ (getLLVMType rettyp) ++ " " ++ tmp)
    SReturnVoid              ->
      do
        getNextTemp
        emit "ret void"
    SWhile exp stmt          ->
      do
        cond_label <- getNextLabel
        body_label <- getNextLabel
        finally_label <- getNextLabel
        genJump cond_label

        genLabel cond_label
        (tmp, typ) <- codeGenExpr exp
        genBranch tmp body_label finally_label

        genLabel body_label
        codeGenStmt stmt rettyp
        genJump cond_label

        genLabel finally_label

    SBlock stmts             ->
      do
        enterScope
        codeGenStmts stmts rettyp
        exitScope
    SIfElse exp stmt1 stmt2  ->
      do
        (tmp, typ) <- codeGenExpr exp
        then_label <- getNextLabel
        else_label <- getNextLabel
        finally_label <- getNextLabel
        genBranch tmp then_label else_label

        genLabel then_label
        codeGenStmt stmt1 rettyp
        genJump finally_label

        genLabel else_label
        codeGenStmt stmt2 rettyp
        genJump finally_label

        genLabel finally_label

genBranch :: LLVMExpr -> LLVMExpr -> LLVMExpr -> State Env ()
genBranch cond true_label false_label =
  emit ("br i1 "++cond++", label %"++true_label++", label %"++false_label)

genJump :: LLVMExpr -> State Env ()
genJump target = emit ("br label %"++target)

genLabel :: LLVMExpr -> State Env ()
genLabel label = emit (label++":")

-- Generate code for Expression
codeGenExpr :: Exp -> State Env (LLVMExpr, LLVMType)
codeGenExpr expr =
  case expr of
    ETrue                    -> return ("1", "i1")
    EFalse                   -> return ("0", "i1")
    EInt value               -> return ((show value), "i32")
    EDouble value            -> return ((show value), "double")
    EString _                -> return ("", "")
    EId id                   ->
      do
        varinfo <- lookupVar id
        tmp <- getNextTemp
        emit (tmp ++ " = load " ++ (typ varinfo) ++ "* " ++ getMangled varinfo)
        return (tmp, (typ varinfo))
    EApp id exprs            ->
      do
        funInfo <- lookupFun id
        tmps <- mapM codeGenExpr exprs
        tmp <- getNextTemp
        emit (tmp ++ " = call " ++ retTyp funInfo ++ " @" ++ mangledF funInfo ++ "(" ++ printArgs tmps ++ ")")
        return (tmp, (retTyp funInfo))
    EPIncr exp               ->
      do
        emit ("; PostIncrement not supported")
        return ("", "")
    EPDecr exp               ->
      do
        emit ("; PostDecrement not supported")
        return ("", "")
    EIncr exp                ->
      do
        emit ("; PreIncrement not supported")
        return ("", "")
    EDecr exp                ->
      do
        emit ("; PreDecrement not supported")
        return ("", "")
    ETimes lhs rhs           -> genBinOpExpr lhs rhs [("i32","mul"), ("double", "fmul")]
    EDiv lhs rhs             -> genBinOpExpr lhs rhs [("i32","sdiv"), ("double", "fdiv")]
    EPlus lhs rhs            -> genBinOpExpr lhs rhs [("i32","add"), ("double", "fadd")]
    EMinus lhs rhs           -> genBinOpExpr lhs rhs [("i32","sub"), ("double", "fsub")]
    ELt lhs rhs              -> genCmpExpr "slt" lhs rhs
    EGt lhs rhs              -> genCmpExpr "sgt" lhs rhs
    ELtEq lhs rhs            -> genCmpExpr "sle" lhs rhs
    EGtEq lhs rhs            -> genCmpExpr "sge" lhs rhs
    EEq lhs rhs              -> genCmpExpr "eq" lhs rhs
    ENEq lhs rhs             -> genCmpExpr "ne" lhs rhs
    EAnd lhs rhs             -> genBinExpr "and" lhs rhs
    EOr lhs rhs              -> genBinExpr "or" lhs rhs
    EAss lhs rhs ->
      case lhs of
        ETyped content typ -> codeGenExpr (EAss content rhs)
        EId lhsid ->
          do
            (rhs, rhtype) <- codeGenExpr rhs
            varinfo <- lookupVar lhsid
            emit (store rhtype rhs (getMangled varinfo))
            return ((getMangled varinfo), (typ varinfo))
        _         -> error $ "unexpected Exp wrapped inside of EAss, only ETyped and EId allowed"
    ETyped exp typ           -> codeGenExpr exp


genCmpExpr :: LLVMExpr -> Exp -> Exp -> State Env (LLVMExpr, LLVMType)
genCmpExpr cond lhs rhs =
  do
    (lhs, typ) <- codeGenExpr lhs
    (rhs, _) <- codeGenExpr rhs
    instr <- (\typ -> if (typ == "i32") then
                        return " = icmp "
                      else
                        return " = fcmp ") typ
    tmp <- getNextTemp
    emit (tmp++instr++cond++" "++typ++" "++lhs++", "++rhs)
    return (tmp, "i1")

genBinExpr :: LLVMExpr -> Exp -> Exp -> State Env (LLVMExpr, LLVMType)
genBinExpr binop lhs rhs =
  do
    (lhs, typ) <- codeGenExpr lhs
    (rhs, _) <- codeGenExpr rhs
    tmp <- getNextTemp
    emit (tmp++" = "++binop++" "++typ++" "++lhs++", "++rhs)
    return (tmp, typ)

-- Generate llvm code for binary oparation, specify Type -> operation mapping in ops list
genBinOpExpr :: Exp -> Exp -> [(String, String)] -> State Env (LLVMExpr, LLVMType)
genBinOpExpr lhs rhs ops =
  do
    (lhs, lhsType) <- codeGenExpr lhs
    (rhs, _) <- codeGenExpr rhs
    tmp <- getNextTemp
    case lookup lhsType ops of
      Nothing -> error $ "type for operation " ++ lhsType ++ " not found"
      Just op -> emit (tmp ++ " = " ++ op ++ " " ++ lhsType ++ " " ++ lhs ++ ", " ++ rhs)
    return (tmp,lhsType)

-------------------------------------------------------------------------------------------------
--LLVM methods
-------------------------------------------------------------------------------------------------

-- Creates unique name of identifier
mangleName :: Id -> State Env LLVMExpr
mangleName (Id name) =
  do
    env <- get
    let newid = getNextMangled name (mangledIds env)
    modify (\env -> env {mangledIds = (newid:(mangledIds env))})
    return newid

getNextMangled :: LLVMExpr -> [LLVMExpr] -> LLVMExpr
getNextMangled id existing =
  if elem id existing then
    getNextMangled (id++"_") existing
  else
    id

-- Get LLVMType representing given type
getLLVMType :: Type -> LLVMType
getLLVMType typ = case typ of
  Type_bool -> "i1"
  Type_int -> "i32"
  Type_double -> "double"
  Type_void -> "void"
  Type_string -> error $ "Strings aren't supported."

-- Create llvm instruction for function definition
define :: Type -> Id -> [VarInfo] -> Instruction
define typ id args = "define " ++ getLLVMType typ ++ " @" ++ printTree id ++ "(" ++ compileArgs args ++ ") {"

-- Create llvm instruction for allocation
allocate :: LLVMType -> LLVMExpr -> Instruction
allocate typ id = id ++ " = alloca " ++ typ

-- Create llvm instruction for store, source must include '%' sign if required
store :: LLVMType -> LLVMExpr -> LLVMExpr -> Instruction
store typ source target = "store " ++ typ ++ " " ++ source ++ ", " ++ typ ++ "* " ++ target

-- Compile list of function arguments to string
compileArgs :: [VarInfo] -> String
compileArgs [] = ""
compileArgs [a] = compileArg a
compileArgs (a:as) = compileArg a ++ "," ++ compileArgs as

-- Compile function argument to string
compileArg :: VarInfo -> String
compileArg info = typ info ++ " " ++ getMangled info

-- print list of arguments for function call
printArgs :: [(LLVMExpr, LLVMType)] -> String
printArgs [] = ""
printArgs [info] = printArg info
printArgs (info:infos) = printArg info ++ ", " ++ printArgs infos
-- print argument for function call
printArg :: (LLVMExpr, LLVMType) -> String
printArg (id, typ) = typ ++ " " ++ id

