module TypeChecker where

import AbsMini
import PrintMini
import ErrM

type Env = [ [ ( Ident , Type ) ] ]
emptyEnv :: Env
emptyEnv = [[]]

addVar :: Env -> Ident -> Type -> Err Env
lookupVar :: Env -> Ident -> Err TypeChecker
addScope :: Env -> Env


typecheck :: Program -> Err ( )
checkStms :: Env -> [ Stm ] -> Err ( )
checkStm  :: Env -> Stm -> Err Env
checkExp  :: Env -> Exp -> Type -> Err ( )
inferExp  :: Env -> Exp -> Err Type
