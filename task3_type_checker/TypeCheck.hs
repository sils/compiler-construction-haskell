module TypeChecker where

import AbsMini
import PrintMini
import ErrM

type Env = [ [ ( Ident , Type ) ] ]
emptyEnv :: Env
emptyEnv = [[]]

addVar :: Env -> Ident -> Type -> Err Env
addVar (scope:rest) identifier typ =
  case lookup identifier scope of
    Nothing -> return (((identifier, typ):scope):rest)
    Just _ -> fail ("Variable " ++ printTree identifier ++ " was already declared.")

lookupVar :: Env -> Ident -> Err TypeChecker
lookupVar [] identifier = fail ("Unknown variable " ++ printTree identifier ++ ".")
lookupVar (scope:rest) identifier =
  case lookup identifier scope of
    Nothing -> lookupVar rest identifier
    Just typ -> return typ

addScope :: Env -> Env
addScope env = []:env

typecheck :: Program -> Err ( )
checkStms :: Env -> [ Stm ] -> Err ( )
checkStm  :: Env -> Stm -> Err Env
checkExp  :: Env -> Exp -> Type -> Err ( )
inferExp  :: Env -> Exp -> Err Type
