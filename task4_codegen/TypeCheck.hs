module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Control.Monad


type VarInfo = (Type, Int)

-- Id of Function with return type and types of arguments
type Fun = (Id, (Type, [Type]))
-- Id of Variable with info about it's type and uses
type Var = (Id, VarInfo)

type Instruction = String

-- CodegenEnvironment contains llvm code, id of next temporary variable and
-- Environment of typeChecker
data GEnv = E {
  nextTmp :: Int,
  code :: [Instruction],
  env :: Env
}
emptyGEnv :: GEnv
emptyGEnv = E {
  nextTmp = 1,
  code = [],
  env = emptyEnv
}

-- update nextTmp property of CodegenEnvironment
createNextTmp :: GEnv -> GEnv
createNextTmp (E nextTmp code tEnv) = E (nextTmp + 1) code tEnv

-- The environment is a list of *scopes*. Each scope holds a list of
-- Identifiers and their associated types.
type Env = [([Fun], [Var])]
emptyEnv :: Env
emptyEnv = [([],[])]

-- Adds a variable to the uppermost scope of the given environment.
addVar :: GEnv -> Id -> Type -> Err GEnv
addVar (E _ _ []) _ _ = Bad ("Can't add a variable to environment without scopes")
addVar (E nextTemp code (scope:rest)) identifier typ =
  case lookup identifier (snd scope) of
    Nothing -> Ok (E nextTemp code ((fst scope, (identifier, (typ, 0)):(snd scope)):rest))
    Just _ -> Bad ("Variable " ++ printTree identifier ++ " was already declared.")

-- Adds several variables with addVar
addVars :: GEnv -> [Id] -> Type -> Err GEnv
addVars gEnv [] typ = Ok gEnv
addVars gEnv (top:rest) typ =
  do
    gEnv_ <- addVar gEnv top typ
    addVars gEnv_ rest typ

-- Adds a function definition to uppermost scope of the given environment
addFun :: GEnv -> Id -> Type -> [ Arg ] -> Err GEnv
addFun (E _ _ []) _ _ _ = Bad ("Can't add a function to environment without scopes")
addFun gEnv@(E nextTmp code (scope:rest)) identifier typ args =
  case lookup identifier (fst scope) of
    Nothing ->
      do
        -- enter function scope
        gEnv_ <- addScope gEnv
        -- add all argument variables to scope
        gEnv__ <- foldM (\gEnv (ADecl typ identifier) -> addVar gEnv identifier typ) gEnv_ args
        -- create function signature
        let sig = (typ, map (\(ADecl typ identifier) -> typ) args)
        -- add function signature to outer scope
        remScope (E nextTmp code ((\(scope:outer:rest) -> scope:(((identifier, sig):(fst outer), snd outer):rest)) (env gEnv__)))
    Just _ -> Bad ("Function " ++ printTree identifier ++ " was already declared.")

addParams :: GEnv -> Id -> Type -> [ Arg ] -> Err GEnv
addParams (E _ _ []) _ _ _ = Bad ("Can't add a function params to environment without scopes")
addParams gEnv@(E nextTmp code (scope:rest)) identifier typ args =
  case lookup identifier (fst scope) of
    Nothing -> Bad ("Function " ++ printTree identifier ++ " was not declared.")
    Just _  ->
      do
        -- enter function scope
        gEnv_ <- addScope gEnv
        -- add all argument variables to scope
        gEnv__ <- foldM (\gEnv (ADecl typ identifier) -> addVar gEnv identifier typ) gEnv_ args
        Ok gEnv__

-- Looks up a variable in the given environment.
lookupVar :: Env -> Id -> Err VarInfo
lookupVar [] identifier = Bad ("Unknown variable " ++ printTree identifier ++ ".")
lookupVar (scope:rest) identifier =
  case lookup identifier (snd scope) of
    Nothing -> lookupVar rest identifier
    Just info -> Ok info

-- Looks up a function in the given environment
lookupFun :: Env -> Id -> Err (Type, [Type])
lookupFun [] identifier = Bad ("Unknown Function " ++ printTree identifier ++ ".")
lookupFun (scope:rest) identifier =
  case lookup identifier (fst scope) of
    Nothing -> lookupFun rest identifier
    Just sig -> Ok sig

-- Adds a new empty scope to the environment.
addScope :: GEnv -> Err GEnv
addScope (E nextTmp code env) = Ok (E nextTmp code (([],[]):env))

remScope :: GEnv -> Err GEnv
remScope (E _ _ []) = Bad "Can't remove scope from environment without scopes"
remScope (E nextTmp code (scope:rest)) = Ok (E nextTmp code rest)


typecheck :: Program -> Err [Instruction]
typecheck (PDefs defs) =
  do
    gEnv <- checkDecls emptyGEnv defs
    gEnv_ <- checkDefs gEnv defs
    Ok (code gEnv_)


checkDecls :: GEnv -> [ Def ] -> Err GEnv
checkDecls gEnv [] = Ok gEnv
checkDecls gEnv (def:defs) =
  do
    -- Why monad needed?
    gEnv_ <- checkDecl gEnv def
    checkDecls gEnv_ defs

checkDecl :: GEnv -> Def -> Err GEnv
checkDecl gEnv def =
  case def of
    DFun typ identifier args stmts -> addFun gEnv identifier typ args

checkDefs :: GEnv -> [ Def ] -> Err GEnv
checkDefs gEnv [] = Ok emptyGEnv
checkDefs gEnv [def] = checkDef gEnv def
checkDefs gEnv (def:defs) =
  do
    -- Why monad needed?
    gEnv_ <- checkDef gEnv def
    gEnv__ <- checkDefs gEnv_ defs
    Ok gEnv__


checkDef :: GEnv -> Def -> Err GEnv
checkDef gEnv def =
  case def of
    -- TODO ask: why do we only use env from addParams and never err?
    DFun typ identifier args stmts ->
      do
        gEnv_ <- addParams gEnv identifier typ args
        gEnv_ <- emit (define typ identifier args) gEnv_
        gEnv_ <- emit "{" gEnv_
        -- TODO - emit allocate for return value
        gEnv_ <- foldM (\gEnv arg -> emitAllocParam arg gEnv) gEnv_ args
        checkStmts gEnv_ stmts typ
        gEnv_ <- emit "}" gEnv_
        gEnv_ <- remScope gEnv_
        Ok gEnv_

checkStmts :: GEnv -> [ Stm ] -> Type -> Err GEnv
checkStmts gEnv [] _ = Ok gEnv
checkStmts gEnv (stmt:stmts) typ =
  do
    gEnv_ <- checkStmt gEnv stmt typ
    checkStmts gEnv_ stmts typ

checkStmt :: GEnv -> Stm -> Type -> Err GEnv
checkStmt gEnv stmt typ =
  case stmt of
    SExp exp                 ->
      do
        checkExp (env gEnv) exp
        Ok gEnv
    SDecls typ identifiers   ->
      do
        gEnv_ <- addVars gEnv identifiers typ
        gEnv_ <- foldM (\gEnv decla -> emit (decl typ decla) gEnv) gEnv_ identifiers
        Ok gEnv_
    SInit typ identifier exp ->
      do
        if (Ok typ == checkExp (env gEnv) exp) then
            addVar gEnv identifier typ
        else
          Bad ("Expression is of wrong type")
    SReturn exp              ->
      do
        checkExpType (env gEnv) exp typ
        Ok gEnv
    SReturnVoid              ->
      Ok gEnv
    SWhile exp stmt          ->
      do
        gEnv_ <- addScope gEnv
        -- Expressions cannot change environment
        if (checkExp (env gEnv_) exp == Ok Type_bool) then
          do
            checkStmt gEnv_ stmt typ
            Ok gEnv
        else
          Bad ("Expression of while loops must be of type boolean")
    SBlock stmts             ->
      do
        gEnv_ <- addScope gEnv
        checkStmts gEnv_ stmts typ
        Ok gEnv
    SIfElse exp stmt1 stmt2  ->
      do
        gEnv_ <- addScope gEnv
        if (checkExp (env gEnv_) exp == Ok Type_bool) then
          do
            checkStmt gEnv_ stmt1 typ
            checkStmt gEnv_ stmt2 typ
            Ok gEnv
        else
          Bad ("Expression in if expressions must be of type boolean")

checkExp :: Env -> Exp -> Err Type
checkExp env exp =
  case exp of
    ETrue                    -> Ok Type_bool
    EFalse                   -> Ok Type_bool
    EInt _                   -> Ok Type_int
    EDouble _                -> Ok Type_double
    EString _                -> Ok Type_string
    EId id                   ->
      case lookupVar env id of
        Bad s -> Bad s
        Ok (typ, _) -> Ok typ
    EApp id exprs            -> 
      do
        (retType, types) <- lookupFun env id
        if (length exprs == length types) then
          do
            mapM (\(expr, typ) -> checkExpType env expr typ) (zip exprs types)
            Ok retType
        else
          Bad ("Number of passed arguments doesn't match function declaration")
    EPIncr exp               -> checkUnaryArithmeticOperator env exp
    EPDecr exp               -> checkUnaryArithmeticOperator env exp
    EIncr exp                -> checkUnaryArithmeticOperator env exp
    EDecr exp                -> checkUnaryArithmeticOperator env exp
    ETimes lhs rhs           -> checkArithmeticOperator env lhs rhs
    EDiv lhs rhs             -> checkArithmeticOperator env lhs rhs
    EPlus lhs rhs            -> checkPlusOperator env lhs rhs
    EMinus lhs rhs           -> checkArithmeticOperator env lhs rhs
    ELt lhs rhs              ->
      do
        checkExpTypeEquality env lhs rhs
        Ok Type_bool
    EGt lhs rhs              ->
      do
        checkExpTypeEquality env lhs rhs
        Ok Type_bool
    ELtEq lhs rhs            ->
      do
        checkExpTypeEquality env lhs rhs
        Ok Type_bool
    EGtEq lhs rhs            ->
      do
        checkExpTypeEquality env lhs rhs
        Ok Type_bool
    EEq lhs rhs              ->
      do
        checkExpTypeEquality env lhs rhs
        Ok Type_bool
    ENEq lhs rhs             ->
      do
        checkExpTypeEquality env lhs rhs
        Ok Type_bool
    EAnd lhs rhs             -> checkExpTypesAreBool env lhs rhs
    EOr lhs rhs              -> checkExpTypesAreBool env lhs rhs
    EAss lhs rhs             -> checkExpTypeEquality env lhs rhs
    ETyped _ typ            -> Ok typ

checkUnaryArithmeticOperator :: Env -> Exp -> Err Type
checkUnaryArithmeticOperator env exp =
  do
    typ <- checkExp env exp
    if (typ == Type_int || typ == Type_double) then
      Ok typ
    else
      Bad ("Unary operators are only defined for int and double")

checkArithmeticOperator :: Env -> Exp -> Exp -> Err Type
checkArithmeticOperator env lhs rhs =
  do
    lhsTyp <- checkExp env lhs
    if (lhsTyp == Type_int || lhsTyp == Type_double) then
      checkExpType env rhs lhsTyp
    else
      Bad ("Arithmetic operator is only definded for types int and double")

checkPlusOperator :: Env -> Exp -> Exp -> Err Type
checkPlusOperator env lhs rhs =
  do
    lhsTyp <- checkExp env lhs
    if (lhsTyp == Type_int || lhsTyp == Type_double || lhsTyp == Type_string) then
      checkExpType env rhs lhsTyp
    else
      Bad ("Arithmetic operator is only definded for types int and double")


-- checks if given expression is of given type
checkExpType :: Env -> Exp -> Type -> Err Type
checkExpType env exp typ =
  case checkExp env exp of
    Ok expTyp ->
      if expTyp == typ then
        Ok expTyp
      else
        Bad ("Types don't match in function call. Exp : " ++ printTree exp ++ " should be of type " ++ printTree typ ++ " but has type " ++ printTree expTyp)
    Bad s -> Bad s

-- checks if types of both given expressions are equal, returns type of expressions or error
checkExpTypeEquality :: Env -> Exp -> Exp -> Err Type
checkExpTypeEquality env lhs rhs =
  do
    typ1 <- checkExp env lhs
    typ2 <- checkExp env rhs
    if (typ1 == typ2) then
      Ok typ1
    else
      Bad ("Types of expressions don't match. lhs: " ++ printTree lhs ++ " of type " ++ printTree typ1 ++ " rhs: " ++ printTree rhs ++ " of type " ++ printTree typ2)

-- checks if types of both given expressions are boolean, returns Type_bool or error
checkExpTypesAreBool :: Env -> Exp -> Exp -> Err Type
checkExpTypesAreBool env lhs rhs =
      if (checkExp env lhs == Ok Type_bool && checkExp env rhs == Ok Type_bool) then
        Ok Type_bool
      else
        Bad ("Types must be boolean in Conjuctions and Disjunctions")

------------------------------------------------------------------------------------------------------------------------------
-- 							CodeGenerator methods
------------------------------------------------------------------------------------------------------------------------------
emits :: [Instruction] -> GEnv -> Err GEnv
emits instrs gEnv = foldM (\gEnv instr -> emit instr gEnv) gEnv instrs

emit :: Instruction -> GEnv -> Err GEnv
emit instr (E nextTmp code env) = Ok (E nextTmp (code ++ [instr]) env)

emitAllocParam :: Arg -> GEnv -> Err GEnv
emitAllocParam (ADecl typ id) gEnv =
  do
    let alloca = emitAlloc typ gEnv
    emit (store typ id (snd alloca)) (fst alloca)

emitAlloc :: Type -> GEnv -> (GEnv, Int)
emitAlloc typ (E nextTemp instrs env_) =
  do
    let instrs_ = instrs ++ (["%" ++ show nextTemp ++ " = alloca " ++ getLLVMType typ])
    ((E (nextTemp + 1) instrs_ env_), nextTemp)

store :: Type -> Id -> Int -> Instruction
store typ id i = "store " ++ getLLVMType typ ++ " %" ++ printTree id ++ ", " ++ getLLVMType typ ++ "* %" ++ show i

define :: Type -> Id -> [Arg] -> Instruction
define typ id args = "define " ++ (getLLVMType typ) ++ " @" ++ (printTree id) ++ " (" ++ compileArgs(args) ++ ") #0"

decl :: Type -> Id -> Instruction
decl typ id = "%" ++ (printTree id) ++ " = alloca " ++ getLLVMType typ

compileArgs :: [Arg] -> String
compileArgs [] = ""
compileArgs [a] = compileArg a
compileArgs (a:as) = (compileArg a) ++ ("," ++ (compileArgs as))

compileArg :: Arg -> String
compileArg (ADecl typ id) = (getLLVMType typ) ++ " %" ++ (printTree id)

getLLVMType :: Type -> String
getLLVMType typ =
  case typ of
    Type_void -> "void"
    Type_bool -> "i1"
    Type_int -> "i32"
    Type_double -> "f64"
 -- Type_string -> not supported

