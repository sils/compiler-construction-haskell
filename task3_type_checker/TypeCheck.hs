module TypeChecker where

import AbsMini
import PrintMini
import ErrM

typecheck :: Program -> Err ( )
checkStms :: Env -> [ Stm ] -> Err ( )
checkStm  :: Env -> Stm -> Err Env
checkExp  :: Env -> Exp -> Type -> Err ( )
inferExp  :: Env -> Exp -> Err Type
