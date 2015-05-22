module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Control.Monad

type Fun = (Id, (Type, [Type]))
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

-- Adds several variables with addVar
addVars :: Env -> [Id] -> Type -> Err Env
addVars env [] typ = Ok env
addVars env (top:rest) typ =
  do
    env_ <- addVar env top typ
    addVars env_ rest typ

-- Adds a function definition to uppermost scope of the given environment
addFun :: Env -> Id -> Type -> [ Arg ] -> Err Env
addFun env@(scope:rest) identifier typ args =
  case lookup identifier (fst scope) of
    Nothing ->
      do
        -- enter function scope
        env_ <- addScope env
        -- add all argument variables to scope
        env__ <- foldM (\env (ADecl typ identifier) -> addVar env identifier typ) env_ args
        -- create function signature
        let sig = (typ, map (\(ADecl typ identifier) -> typ) args)
        -- add function signature to outer scope
        Ok ((\(scope:outer:rest) -> scope:(((identifier, sig):(fst outer), snd outer):rest)) env__)
    Just _ -> Bad ("Function " ++ printTree identifier ++ " was already declared.")

-- Looks up a variable in the given environment.
lookupVar :: Env -> Id -> Err Type
lookupVar [] identifier = Bad ("Unknown variable " ++ printTree identifier ++ ".")
lookupVar (scope:rest) identifier =
  case lookup identifier (snd scope) of
    Nothing -> lookupVar rest identifier
    Just typ -> Ok typ

-- Looks up a function in the given environment
lookupFun :: Env -> Id -> Err (Type, [Type])
loopupFun [] identifier = Bad ("Unknown variable" ++ printTree identifier ++ ".")
lookupFun (scope:rest) identifier =
  case lookup identifier (fst scope) of
    Nothing -> lookupFun rest identifier
    Just sig -> Ok sig

-- Adds a new empty scope to the environment.
addScope :: Env -> Err Env
addScope env = Ok ([],[]):env

remScope :: Env -> Err Env
remScope [] = Bad []
remScope (scope:rest) = Ok rest


typecheck :: Program -> Err ()
typecheck (PDefs defs) = checkDefs emptyEnv defs

checkDefs :: Env -> [ Def ] -> Err ()
checkDefs env [] = Ok ()
checkDefs env (def:defs) =
  do
    -- Why monad needed?
    env_ <- checkDef env def
    checkDefs env_ defs


checkDef :: Env -> Def -> Err Env
checkDef env def = 
  case def of
    -- TODO ask: why do we only use env from addFun and never err?
    DFun typ identifier args stmts ->
      do
        env_ <- addFun env identifier typ args
        checkStmts env_ stmts
        env__ <- remScope env_
        Ok env__

checkStmts :: Env -> [ Stm ] -> Err Env
checkStmts env [] = Ok env
checkStmts env (stmt:stmts) =
  do
    env_ <- checkStmt env stmt
    checkStmts env_ stmts

checkStmt :: Env -> Stm -> Err Env
checkStmt env stmt =
  case stmt of
    SExp exp                 ->
      do
        checkExp env exp
        Ok env
    SDecls typ identifiers   ->
      addVars env identifiers typ
    SInit typ identifier exp ->
      do
        if (Ok typ == checkExp env exp) then
          addVar env identifier typ
        else
          Bad ("Expression is of wrong type")
    SReturn exp              ->
      do
        checkExp env exp
        Ok env
    SReturnVoid              ->
      Ok env
    SWhile exp stmt          ->
      do
        env_ <- addScope env
        -- Expressions cannot change environment
        checkExp env_ exp
        checkStmt env_ stmt
        Ok env
    SBlock stmts             ->
      do
        env_ <- addScope env
        checkStmts env_ stmts
        Ok env
    SIfElse exp stmt1 stmt2  ->
      do
        env_ <- addScope env
        checkExp env_ exp
        checkStmt env_ stmt1
        checkStmt env_ stmt2
        Ok env

checkExp :: Env -> Exp -> Err Type
checkExp env exp =
  case exp of
    ETrue                    -> Ok Type_bool
    EFalse                   -> Ok Type_bool
    EInt _                   -> Ok Type_int
    EDouble _                -> Ok Type_double
    EId _                    -> Ok Type_void
    EApp id exprs            -> Ok Type_void -- TODO - check function declaration, return return type of function
    EPIncr exp               -> checkExp env exp
    EPDecr exp               -> checkExp env exp
    ETimes lhs rhs           -> checkExpTypeEquality env lhs rhs
    EDiv lhs rhs             -> checkExpTypeEquality env lhs rhs
    EPlus lhs rhs            -> checkExpTypeEquality env lhs rhs
    EMinus lhs rhs           -> checkExpTypeEquality env lhs rhs
    ELt lhs rhs              -> checkExpTypeEquality env lhs rhs
    EGt lhs rhs              -> checkExpTypeEquality env lhs rhs
    ELtEq lhs rhs            -> checkExpTypeEquality env lhs rhs
    EGtEq lhs rhs            -> checkExpTypeEquality env lhs rhs
    EEq lhs rhs              -> checkExpTypeEquality env lhs rhs
    ENEq lhs rhs             -> checkExpTypeEquality env lhs rhs
    EAnd lhs rhs             -> checkExpTypesAreBool env lhs rhs
    EOr lhs rhs              -> checkExpTypesAreBool env lhs rhs
    EAss lhs rhs             -> checkExpTypeEquality env lhs rhs

-- checks if types of both given expressions are equal, returns type of expressions or error
checkExpTypeEquality :: Env -> Exp -> Exp -> Err Type
checkExpTypeEquality env lhs rhs =
  do
    typ1 <- checkExp env lhs
    typ2 <- checkExp env rhs
    if (typ1 == typ2) then
      Ok typ1
    else
      Bad ("Types don't match")

-- checks if types of both given expressions are boolean, returns Type_bool or error
checkExpTypesAreBool :: Env -> Exp -> Exp -> Err Type
checkExpTypesAreBool env lhs rhs =
      if (checkExp env lhs == Ok Type_bool && checkExp env rhs == Ok Type_bool) then
        Ok Type_bool
      else
        Bad ("Types must be boolean in Conjuctions and Disjunctions")



