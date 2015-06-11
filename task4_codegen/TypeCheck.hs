module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Control.Monad

import Debug.Trace

type Fun = (Id, (Type, [Type]))
type Var = (Id, Type)

-- The environment is a list of *scopes*. Each scope holds a list of
-- Identifiers and their associated types.
type Env = [([Fun], [Var])]
emptyEnv :: Env
emptyEnv = [([],[])]


------------------------------------------------------------------------------
--TypeChecker
-----------------------------------------------------------------------------

-- TypeCheck given program
-- adds all function declarations to outer scope
-- then typeChecks all functions
typecheck :: Program -> Err ()
typecheck (PDefs defs) =
  do
    env <- checkDecls emptyEnv defs
    checkDefs env defs

------------------------------------------------------------------------------
--Environment Methods
-----------------------------------------------------------------------------

-- Adds a new empty scope to the environment.
addScope :: Env -> Err Env
addScope env = Ok (([],[]):env)

-- Remove outermost scope of environment
remScope :: Env -> Err Env
remScope [] = Bad []
remScope (scope:rest) = Ok rest

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

-- enters scope of given function, adds all parameters of function to new scope
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
        foldM (\env (ADecl typ identifier) -> addVar env identifier typ) env_ args

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

-------------------------------------------------------------------------------------
--Methods for first run thru tree, Adds function definitions to environment
-------------------------------------------------------------------------------------

-- Check a list of declarations
checkDecls :: Env -> [ Def ] -> Err Env
checkDecls env defs = foldM (\env def -> checkDecl env def) env defs

-- Check a declaration for validity
checkDecl :: Env -> Def -> Err Env
checkDecl env def =
  case def of
    DFun typ identifier args stmts -> do
      addFun env identifier typ args

-------------------------------------------------------------------------------------
--Methods for second run, Traverse AST and TypeCheck expressions
-------------------------------------------------------------------------------------

-- Check a list of definitions
checkDefs :: Env -> [ Def ] -> Err ()
-- TODO - return Err Program, Err (Env, Stm/Exp/..) in all other methods 
checkDefs env defs = foldM_ (\env def -> checkDef env def) env defs

-- Check definition
checkDef :: Env -> Def -> Err Env
checkDef env def =
  case def of
    DFun typ identifier args stmts ->
      do
        env_ <- addParams env identifier typ args
        env_ <- checkStmts env_ stmts typ
        remScope env_

-- check a list of statements
checkStmts :: Env -> [ Stm ] -> Type -> Err Env
checkStmts env stmts typ = foldM (\env stmt -> checkStmt env stmt typ) env stmts

-- check a statement for validity
checkStmt :: Env -> Stm -> Type -> Err Env
checkStmt env stmt typ =
  case stmt of
    SExp exp                 ->
      do
        -- TODO - Return Err (Exp, Env) and similar in all functions
        aExp <- checkExp env exp
        Ok env
    SDecls typ identifiers   ->
      do
        addVars env identifiers typ
    SInit typ identifier exp ->
      do
        aExp <- checkExp env exp
        expTyp <- getTyp aExp
        if (expTyp == typ) then
          do
            addVar env identifier expTyp
        else
          Bad ("Expression is of wrong type in initialization " ++ printTree stmt)
    SReturn exp              ->
      do
        aExp <- checkExpType env exp typ
        Ok env
    SReturnVoid              ->
      Ok env
    SWhile exp stmt          ->
      do
        env_ <- addScope env
        aExp <- checkExp env_ exp
        expTyp <- getTyp aExp
        if (expTyp == Type_bool) then
          do
            env_ <- checkStmt env_ stmt typ
            remScope env_
        else
          Bad ("Expression of while loops must be of type boolean")
    SBlock stmts             ->
      do
        env_ <- addScope env
        env_ <- checkStmts env_ stmts typ
        remScope env_
    SIfElse exp stmt1 stmt2  ->
      do
        env_ <- addScope env
        aExp <- checkExp env_ exp
        expTyp <- getTyp aExp
        if (expTyp == Type_bool) then
          do
            env_ <- checkStmt env_ stmt1 typ
            env_ <- checkStmt env_ stmt2 typ
            remScope env_
        else
          Bad ("Expression in if expressions must be of type boolean")

-- Infer type of expression
checkExp :: Env -> Exp -> Err Exp
checkExp env exp =
  case exp of
    ETrue                    -> Ok (ETyped exp Type_bool)
    EFalse                   -> Ok (ETyped exp Type_bool)
    EInt _                   -> Ok (ETyped exp Type_int)
    EDouble _                -> Ok (ETyped exp Type_double)
    EString _                -> Ok (ETyped exp Type_string)
    EId id                   ->
      do
        typ <- lookupVar env id
        Ok (ETyped exp typ)
    EApp id exprs            -> 
      do
        (retType, types) <- lookupFun env id
        if (length exprs == length types) then
          do
            mapM (\(expr, typ) -> checkExpType env expr typ) (zip exprs types)
            Ok (ETyped exp retType)
        else
          Bad ("Number of passed arguments doesn't match function declaration")
    EPIncr exp               ->
      do
        typ <- checkUnaryArithmeticOperator env exp
        Ok (ETyped exp typ)
    EPDecr exp               ->
      do
        typ <- checkUnaryArithmeticOperator env exp
        Ok (ETyped exp typ)
    EIncr exp                ->
      do
        typ <- checkUnaryArithmeticOperator env exp
        Ok (ETyped exp typ)
    EDecr exp                ->
      do
        typ <- checkUnaryArithmeticOperator env exp
        Ok (ETyped exp typ)
    ETimes lhs rhs           ->
      do
        typ <- checkArithmeticOperator env lhs rhs
        Ok (ETyped (ETimes (ETyped lhs typ) (ETyped rhs typ)) typ)
    EDiv lhs rhs             ->
      do
        typ <- checkArithmeticOperator env lhs rhs
        Ok (ETyped (ETimes (ETyped lhs typ) (ETyped rhs typ)) typ)
    EPlus lhs rhs            ->
      do
        typ <- checkPlusOperator env lhs rhs
        Ok (ETyped (EPlus (ETyped lhs typ) (ETyped rhs typ)) typ)
    EMinus lhs rhs           ->
      do
        typ <- checkArithmeticOperator env lhs rhs
        Ok (ETyped (ETimes (ETyped lhs typ) (ETyped rhs typ)) typ)
    ELt lhs rhs              ->
      do
        checkExpTypeEquality env lhs rhs
        Ok (ETyped exp Type_bool)
    EGt lhs rhs              ->
      do
        checkExpTypeEquality env lhs rhs
        Ok (ETyped exp Type_bool)
    ELtEq lhs rhs            ->
      do
        checkExpTypeEquality env lhs rhs
        Ok (ETyped exp Type_bool)
    EGtEq lhs rhs            ->
      do
        checkExpTypeEquality env lhs rhs
        Ok (ETyped exp Type_bool)
    EEq lhs rhs              ->
      do
        checkExpTypeEquality env lhs rhs
        Ok (ETyped exp Type_bool)
    ENEq lhs rhs             ->
      do
        checkExpTypeEquality env lhs rhs
        Ok (ETyped exp Type_bool)
    EAnd lhs rhs             ->
      do
        checkExpTypesAreBool env lhs rhs
        Ok (ETyped (EAnd (ETyped lhs Type_bool) (ETyped rhs Type_bool)) Type_bool)
    EOr lhs rhs              ->
      do
        checkExpTypesAreBool env lhs rhs
        Ok (ETyped (EAnd (ETyped lhs Type_bool) (ETyped rhs Type_bool)) Type_bool)
    EAss lhs rhs             ->
      do
        typ <- checkExpTypeEquality env lhs rhs
        Ok (ETyped (EAss (ETyped lhs typ) (ETyped rhs typ)) typ)
    ETyped _ typ            -> Ok exp

-- TODO - need to return nested Exp as ETyped as well, use Err Exp and Err (Exp, Exp) as return type where Exp is a ETyped Exp Type
-- checks if given expression is of given type
checkExpType :: Env -> Exp -> Type -> Err Exp
checkExpType env exp typ =
  case checkExp env exp of
    Ok aExp ->
      do
        isTyp <- getTyp aExp
        if isTyp == typ then
          Ok (ETyped exp typ)
        else
          Bad ("Type mismatch. Exp : " ++ printTree exp ++ " should be of type " ++ printTree typ ++ " but has type " ++ printTree isTyp)
    Bad s -> Bad s

-- checks if types of both given expressions are equal, returns type of expressions or error
checkExpTypeEquality :: Env -> Exp -> Exp -> Err Type
checkExpTypeEquality env lhs rhs =
  do
    typ1 <- checkExp env lhs
    typ2 <- checkExp env rhs
    if (getTyp typ1 == getTyp typ2) then
      getTyp typ1
    else
      Bad ("Types of expressions don't match. lhs: " ++ printTree lhs ++ " of type " ++ printTree typ1 ++ " rhs: " ++ printTree rhs ++ " of type " ++ printTree typ2)

-- checks if types of both given expressions are boolean, returns Type_bool or error
checkExpTypesAreBool :: Env -> Exp -> Exp -> Err ()
checkExpTypesAreBool env lhs rhs =
  do
    typ1 <- checkExp env lhs
    typ2 <- checkExp env rhs
    if (getTyp typ1 == Ok Type_bool && getTyp typ2 == Ok Type_bool) then
      Ok ()
    else
      Bad ("Types must be boolean in Conjuctions and Disjunctions")

-- infer type of unary operator
checkUnaryArithmeticOperator :: Env -> Exp -> Err Type
checkUnaryArithmeticOperator env exp =
  do
    aExp <- checkExp env exp
    typ <- getTyp aExp
    if (typ == Type_int || typ == Type_double) then
      Ok typ
    else
      Bad ("Unary operators are only defined for int and double")

-- infer type of arithmetic operator
checkArithmeticOperator :: Env -> Exp -> Exp -> Err Type
checkArithmeticOperator env lhs rhs =
  do
    aExpLhs <- checkExp env lhs
    lhsTyp <- getTyp aExpLhs
    if (lhsTyp == Type_int || lhsTyp == Type_double) then
      do
        aExpRhs <- checkExpType env rhs lhsTyp
        getTyp aExpRhs
    else
      Bad ("Arithmetic operator is only definded for types int and double")

-- infer type of + operator
checkPlusOperator :: Env -> Exp -> Exp -> Err Type
checkPlusOperator env lhs rhs =
  do
    aExpLhs <- checkExp env lhs
    lhsTyp <- getTyp aExpLhs
    if (lhsTyp == Type_int || lhsTyp == Type_double || lhsTyp == Type_string) then
      do
        aExpRhs <- checkExpType env rhs lhsTyp
        getTyp aExpRhs
    else
      Bad ("Arithmetic operator is only definded for types int and double")

-- Get Type wrapped in ETyped
getTyp :: Exp -> Err Type
getTyp exp =
  case exp of
    ETyped _ typ -> Ok typ
    -- should not be called for other exps'


