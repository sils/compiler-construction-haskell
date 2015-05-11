module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM

type Fun = (Id, Type, [Type])
type Var = (Id, Type)

-- The environment is a list of *scopes*. Each scope holds a list of
-- Identifiers and their associated types.
type Env = [([Fun], [Var])]
emptyEnv :: Env
emptyEnv = [([],[])]

-- Adds a variable to the uppermost scope of the given environment.
addVar :: Env -> Id -> Type -> Err Env
addVar (scope:rest) identifier typ =
  case lookup identifier (snd scope) of
    Nothing -> Ok ((fst scope, (identifier, typ):(snd scope)):rest)
    Just _ -> Bad ("Variable " ++ printTree identifier ++ " was already declared.")

-- Adds a function definition to uppermost scope of the given environment
addFun :: Env -> Id -> Type -> [ Arg ] -> Err Env
addFun (scope:rest) identifier typ args =
  case lookup identifier (fst scope) of
    Nothing -> 
    Just _ -> Bad ("Function " ++ printTree identifier ++ " was already declared.")

-- Looks up a variable in the given environment.
lookupVar :: Env -> Id -> Err Type
lookupVar [] identifier = Bad ("Unknown variable " ++ printTree identifier ++ ".")
lookupVar (scope:rest) identifier =
  case lookup identifier (snd scope) of
    Nothing -> lookupVar rest identifier
    Just typ -> Ok typ

-- Adds a new empty scope to the environment.
addScope :: Env -> Env
addScope env = ([],[]):env


typecheck :: Program -> Err ()
typecheck (PDefs defs) = checkDefs emptyEnv defs

checkDefs :: Env -> [ Def ] -> Err ( )
checkDefs env [] = Ok ()
checkDefs env (def:defs) =
  do
    -- Why monad needed?
    env_ <- checkDef env def
    checkDefs env_ defs


checkDef :: Env -> Def -> Err Env
checkDef env def = 
  case def of
    DFun typ identifier args stmts -> do env_ <- addFun env identifier typ args
                                         checkStmts env_ stmts
                                         Ok env_

checkStmts :: Env -> [ Stm ] -> Err Env
-- TODO check all statements

