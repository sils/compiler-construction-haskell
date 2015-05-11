module TypeChecker where

import AbsMini
import PrintMini
import ErrM

-- The environment is a list of *scopes*. Each scope holds a list of
-- Identifiers and their associated types.
type Env = [ [ ( Ident , Type ) ] ]
emptyEnv :: Env
emptyEnv = [[]]

-- Adds a variable to the uppermost scope of the given environment.
addVar :: Env -> Ident -> Type -> Err Env
addVar (scope:rest) identifier typ =
  case lookup identifier scope of
    Nothing -> return (((identifier, typ):scope):rest)
    Just _ -> fail ("Variable " ++ printTree identifier ++ " was already declared.")

-- Looks up a variable in the given environment.
lookupVar :: Env -> Ident -> Err TypeChecker
lookupVar [] identifier = fail ("Unknown variable " ++ printTree identifier ++ ".")
lookupVar (scope:rest) identifier =
  case lookup identifier scope of
    Nothing -> lookupVar rest identifier
    Just typ -> return typ

-- Adds a new empty scope to the environment.
addScope :: Env -> Env
addScope env = []:env


typecheck :: Program -> Err ( )
checkStms :: Env -> [ Stm ] -> Err ( )
checkStm  :: Env -> Stm -> Err Env
checkExp  :: Env -> Exp -> Type -> Err ( )
inferExp  :: Env -> Exp -> Err Type
