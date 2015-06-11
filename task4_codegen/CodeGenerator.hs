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
-- Variable information: Name, typ and number of uses (for loads from stack)
data VarInfo = VI {
  mangled :: LLVMExpr,
  typ :: LLVMType,
  uses :: Int
}

-------------------------------------------------------------------------------------------------
--Environment methods
-------------------------------------------------------------------------------------------------

-- Environment, nextTemp identifier, code, vars and functions
data Env = E {
  nextTemp :: LLVMExpr,
  code :: [Instruction],
  vars :: [[(Id, VarInfo)]]
}
initEnv :: Env
initEnv = E {
  nextTemp = "1",
  code = [],
  vars = [[]]
}

-- get Identifier of next temporary variable, updates nextTemp in environment
getNextTemp :: State Env LLVMExpr
getNextTemp = do
  env <- get
  let tmp = nextTemp env
  modify (\env -> env {nextTemp = show (read (nextTemp env) + 1)})
  return tmp

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
addVar :: Id -> Type -> State Env ()
addVar id typ = modify (\env -> env {
  vars = case vars env of {
    (scope:rest) -> ((id, VI (mangleName id) (getLLVMType typ) 0) : scope) : rest;
    _            -> [[(id, VI (mangleName id) (getLLVMType typ) 0)]]
  }
})

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

-- emits instruction to environment
emit :: Instruction -> State Env ()
emit i = modify (\env -> env {code = code env ++ [i]})

-------------------------------------------------------------------------------------------------
--CodeGenerator methods
-------------------------------------------------------------------------------------------------

-- Generate code for Program
codeGen :: Program -> [Instruction]
codeGen (PDefs defs) = code $ execState (codeGenDefs defs) initEnv

-- Generate code for Definitions
codeGenDefs :: [Def] -> State Env ()
codeGenDefs defs = mapM_ codeGenDef defs

-- Generate code for Definition
codeGenDef :: Def -> State Env ()
-- TODO - allocate return type??
codeGenDef (DFun typ id args stmts) = do
  enterScope
  -- add arguments to current sope
  mapM_ (\(ADecl typ id) -> addVar id typ) args
  infos <- mapM (\(ADecl _ id) -> lookupVar id) args
  emit (define typ id infos)
  mapM_ codeGenArg args
  codeGenStmts stmts
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
  emit (store (typ info) ("%" ++ mangled info) tmp)
  return ()

-- Generate code for Statements
codeGenStmts :: [Stm] -> State Env ()
codeGenStmts stmts = mapM_ codeGenStmt stmts

-- Generate code for Statement
codeGenStmt :: Stm -> State Env ()
-- TODO - Pattern match stm
codeGenStmt stm = return ()

-- Generate code for Expression
codeGenExpr :: Exp -> State Env LLVMExpr
-- TODO - Pattern match expr, generate code, return identifier of nextTemp
codeGenExpr expr = return ""

-------------------------------------------------------------------------------------------------
--LLVM methods
-------------------------------------------------------------------------------------------------

-- Creates unique name of identifier
mangleName :: Id -> LLVMExpr
-- TODO - actually mangle name
mangleName (Id name) = name

-- Get LLVMType representing given type
getLLVMType :: Type -> LLVMType
getLLVMType typ = case typ of
  Type_bool -> "i1"
  Type_int -> "i32"
  Type_double -> "f64"
  Type_void -> "void"
  Type_string -> error $ "Strings aren't supported."

-- Create llvm instruction for function definition
define :: Type -> Id -> [VarInfo] -> Instruction
define typ id args = "define " ++ getLLVMType typ ++ " @" ++ mangleName id ++ "(" ++ compileArgs args ++ ") {"

-- Create llvm instruction for allocation
allocate :: LLVMType -> LLVMExpr -> Instruction
allocate typ id = "%" ++ id ++ " = alloca " ++ typ

-- Create llvm instruction for store, source must include '%' sign if required
store :: LLVMType -> LLVMExpr -> LLVMExpr -> Instruction
store typ source target = "store " ++ typ ++ " " ++ source ++ ", " ++ typ ++ "* %" ++ target

-- Compile list of function arguments to string
compileArgs :: [VarInfo] -> String
compileArgs [] = ""
compileArgs [a] = compileArg a
compileArgs (a:as) = compileArg a ++ "," ++ compileArgs as

-- Compile function argument to string
compileArg :: VarInfo -> String
compileArg info = typ info ++ " %" ++ mangled info


