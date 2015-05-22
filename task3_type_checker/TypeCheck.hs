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
addVar [] _ _ = Bad ("Can't add a variable to environment without scopes")
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
addFun [] _ _ _ = Bad ("Can't add a function to environment without scopes")
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
        remScope ((\(scope:outer:rest) -> scope:(((identifier, sig):(fst outer), snd outer):rest)) env__)
    Just _ -> Bad ("Function " ++ printTree identifier ++ " was already declared.")

addParams :: Env -> Id -> Type -> [ Arg ] -> Err Env
addParams [] _ _ _ = Bad ("Can't add a function params to environment without scopes")
addParams env@(scope:rest) identifier typ args =
  case lookup identifier (fst scope) of
    Nothing -> Bad ("Function " ++ printTree identifier ++ " was not declared.")
    Just _  ->
      do
        -- enter function scope
        env_ <- addScope env
        -- add all argument variables to scope
        env__ <- foldM (\env (ADecl typ identifier) -> addVar env identifier typ) env_ args
        Ok env__

-- Looks up a variable in the given environment.
lookupVar :: Env -> Id -> Err Type
lookupVar [] identifier = Bad ("Unknown variable " ++ printTree identifier ++ ".")
lookupVar (scope:rest) identifier =
  case lookup identifier (snd scope) of
    Nothing -> lookupVar rest identifier
    Just typ -> Ok typ

-- Looks up a function in the given environment
lookupFun :: Env -> Id -> Err (Type, [Type])
lookupFun [] identifier = Bad ("Unknown Function " ++ printTree identifier ++ ".")
lookupFun (scope:rest) identifier =
  case lookup identifier (fst scope) of
    Nothing -> lookupFun rest identifier
    Just sig -> Ok sig

-- Adds a new empty scope to the environment.
addScope :: Env -> Err Env
addScope env = Ok (([],[]):env)

remScope :: Env -> Err Env
remScope [] = Bad []
remScope (scope:rest) = Ok rest


typecheck :: Program -> Err ()
typecheck (PDefs defs) =
  do
    env <- checkDecls emptyEnv defs
    checkDefs env defs


checkDecls :: Env -> [ Def ] -> Err Env
checkDecls env [] = Ok env
checkDecls env (def:defs) =
  do
    -- Why monad needed?
    env_ <- checkDecl env def
    checkDecls env_ defs

checkDecl :: Env -> Def -> Err Env
checkDecl env def =
  case def of
    DFun typ identifier args stmts -> addFun env identifier typ args

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
    -- TODO ask: why do we only use env from addParams and never err?
    DFun typ identifier args stmts ->
      do
        env_ <- addParams env identifier typ args
        checkStmts env_ stmts typ
        env__ <- remScope env_
        Ok env__

checkStmts :: Env -> [ Stm ] -> Type -> Err Env
checkStmts env [] _ = Ok env
checkStmts env (stmt:stmts) typ =
  do
    env_ <- checkStmt env stmt typ
    checkStmts env_ stmts typ

checkStmt :: Env -> Stm -> Type -> Err Env
checkStmt env stmt typ =
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
        checkExpType env exp typ
        Ok env
    SReturnVoid              ->
      Ok env
    SWhile exp stmt          ->
      do
        env_ <- addScope env
        -- Expressions cannot change environment
        checkExp env_ exp
        checkStmt env_ stmt typ
        Ok env
    SBlock stmts             ->
      do
        env_ <- addScope env
        checkStmts env_ stmts typ
        Ok env
    SIfElse exp stmt1 stmt2  ->
      do
        env_ <- addScope env
        checkExp env_ exp
        checkStmt env_ stmt1 typ
        checkStmt env_ stmt2 typ
        Ok env

checkExp :: Env -> Exp -> Err Type
checkExp env exp =
  case exp of
    ETrue                    -> Ok Type_bool
    EFalse                   -> Ok Type_bool
    EInt _                   -> Ok Type_int
    EDouble _                -> Ok Type_double
    EString _                -> Ok Type_string
    EId id                   -> lookupVar env id
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



