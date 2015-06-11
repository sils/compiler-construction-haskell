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
typecheck :: Program -> Err Program
typecheck (PDefs defs) =
  do
    env <- checkDecls emptyEnv defs
    (env, aDefs) <- checkDefs env defs
    Ok (PDefs aDefs)

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
checkDefs :: Env -> [ Def ] -> Err (Env, [Def])
checkDefs env [] = Ok (env, [])
checkDefs env [def] =
  do
    (env_, aDef) <- checkDef env def
    Ok (env_, [aDef])
checkDefs env (def:defs) =
  do
    (env_, aDef) <- checkDef env def
    (env_, aDefs) <- checkDefs env_ defs
    Ok (env_, aDef : aDefs)

-- Check definition
checkDef :: Env -> Def -> Err (Env, Def)
checkDef env def =
  case def of
    DFun typ identifier args stmts ->
      do
        env_ <- addParams env identifier typ args
        (env_, aStmts) <- checkStmts env_ stmts typ
        env_ <- remScope env_
        Ok (env_, DFun typ identifier args aStmts)

-- check a list of statements
checkStmts :: Env -> [ Stm ] -> Type -> Err (Env, [Stm])
checkStmts env [] typ = Ok (env, [])
checkStmts env [stmt] typ =
  do
    (env_, aStmt) <-checkStmt env stmt typ
    Ok (env_, [aStmt])
checkStmts env (stmt:stmts) typ =
  do
    (env_, aStmt) <- checkStmt env stmt typ
    (env_, aStmts) <- checkStmts env_ stmts typ
    Ok (env_, aStmt : aStmts)

-- check a statement for validity
checkStmt :: Env -> Stm -> Type -> Err (Env, Stm)
checkStmt env stmt typ =
  case stmt of
    SExp exp                 ->
      do
        aExp <- checkExp env exp
        Ok (env, SExp aExp)
    SDecls typ identifiers   ->
      do
        env_ <- addVars env identifiers typ
        Ok (env_, stmt)
    SInit typ identifier exp ->
      do
        aExp <- checkExp env exp
        expTyp <- getTyp aExp
        if (expTyp == typ) then
          do
            env_ <- addVar env identifier expTyp
            Ok (env_, SInit typ identifier aExp)
        else
          Bad ("Expression is of wrong type in initialization " ++ printTree stmt)
    SReturn exp              ->
      do
        aExp <- checkExpType env exp typ
        Ok (env, SReturn aExp)
    SReturnVoid              ->
      Ok (env, stmt)
    SWhile exp stmt          ->
      do
        env_ <- addScope env
        aExp <- checkExp env_ exp
        expTyp <- getTyp aExp
        if (expTyp == Type_bool) then
          do
            (env_, aStmt) <- checkStmt env_ stmt typ
            env_ <- remScope env_
            Ok (env_, SWhile aExp aStmt)
        else
          Bad ("Expression of while loops must be of type boolean")
    SBlock stmts             ->
      do
        env_ <- addScope env
        (env_, aStmts) <- checkStmts env_ stmts typ
        env_ <- remScope env_
        Ok (env_, SBlock aStmts)
    SIfElse exp stmt1 stmt2  ->
      do
        env_ <- addScope env
        aExp <- checkExp env_ exp
        expTyp <- getTyp aExp
        if (expTyp == Type_bool) then
          do
            (env_, aStmt1) <- checkStmt env_ stmt1 typ
            (env_, aStmt2) <- checkStmt env_ stmt2 typ
            env_ <- remScope env_
            Ok (env_, SIfElse aExp aStmt1 aStmt2)
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
            aExprs <- mapM (\(expr, typ) -> checkExpType env expr typ) (zip exprs types)
            Ok (ETyped (EApp id aExprs) retType)
        else
          Bad ("Number of passed arguments doesn't match function declaration")
    EPIncr exp               ->
      do
        aExp <- checkUnaryOperator env exp [Type_int, Type_double] "PostIncrement"
        typ <- getTyp aExp
        Ok (ETyped (EPIncr aExp) typ)
    EPDecr exp               ->
      do
        aExp <- checkUnaryOperator env exp [Type_int, Type_double] "PostDecrement"
        typ <- getTyp aExp
        Ok (ETyped (EPDecr aExp) typ)
    EIncr exp                ->
      do
        aExp <- checkUnaryOperator env exp [Type_int, Type_double] "PreIncrement"
        typ <- getTyp aExp
        Ok (ETyped (EIncr aExp) typ)
    EDecr exp                ->
      do
        aExp <- checkUnaryOperator env exp [Type_int, Type_double] "PreDecrement"
        typ <- getTyp aExp
        Ok (ETyped (EDecr aExp) typ)
    ETimes lhs rhs           ->
      do
        (aLhs, aRhs) <- checkBinaryOperator env lhs rhs [Type_int, Type_double] "Times (*)"
        typ <- getTyp aLhs
        Ok (ETyped (ETimes aLhs aRhs) typ)
    EDiv lhs rhs             ->
      do
        (aLhs, aRhs) <- checkBinaryOperator env lhs rhs [Type_int, Type_double] "Div (/)"
        typ <- getTyp aLhs
        Ok (ETyped (EDiv aLhs aRhs) typ)
    EPlus lhs rhs            ->
      do
        (aLhs, aRhs) <- checkBinaryOperator env lhs rhs [Type_int, Type_double, Type_string] "Plus (+)"
        typ <- getTyp aLhs
        Ok (ETyped (EPlus aLhs aRhs) typ)
    EMinus lhs rhs           ->
      do
        (aLhs, aRhs) <- checkBinaryOperator env lhs rhs [Type_int, Type_double] "Minus (-)"
        typ <- getTyp aLhs
        Ok (ETyped (EMinus aLhs aRhs) typ)
    ELt lhs rhs              ->
      do
        (aLhs, aRhs) <- checkBinaryOperator env lhs rhs [Type_int, Type_double, Type_string] "Lt (<)"
        Ok (ETyped (ELt aLhs aRhs) Type_bool)
    EGt lhs rhs              ->
      do
        (aLhs, aRhs) <- checkBinaryOperator env lhs rhs [Type_int, Type_double, Type_string] "Gt (>)"
        Ok (ETyped (EGt aLhs aRhs) Type_bool)
    ELtEq lhs rhs            ->
      do
        (aLhs, aRhs) <- checkBinaryOperator env lhs rhs [Type_int, Type_double, Type_string] "LtEq (<=)"
        Ok (ETyped (ELtEq aLhs aRhs) Type_bool)
    EGtEq lhs rhs            ->
      do
        (aLhs, aRhs) <- checkBinaryOperator env lhs rhs [Type_int, Type_double, Type_string] "GtEq (>=)"
        Ok (ETyped (EGtEq aLhs aRhs) Type_bool)
    EEq lhs rhs              ->
      do
        (aLhs, aRhs) <- checkBinaryOperator env lhs rhs [Type_int, Type_double, Type_string] "Eq (==)"
        Ok (ETyped (EEq aLhs aRhs) Type_bool)
    ENEq lhs rhs             ->
      do
        (aLhs, aRhs) <- checkBinaryOperator env lhs rhs [Type_int, Type_double, Type_string] "NEq (!=)"
        Ok (ETyped (ENEq aLhs aRhs) Type_bool)
    EAnd lhs rhs             ->
      do
        (aLhs, aRhs) <- checkBinaryOperator env lhs rhs [Type_bool] "And (&&)"
        Ok (ETyped (EAnd aLhs aRhs) Type_bool)
    EOr lhs rhs              ->
      do
        (aLhs, aRhs) <- checkBinaryOperator env lhs rhs [Type_bool] "Or (||)"
        Ok (ETyped (EAnd aLhs aRhs) Type_bool)
    EAss lhs rhs             ->
      do
        (aLhs, aRhs) <- checkBinaryOperator env lhs rhs [Type_int, Type_double, Type_string] "Assignment (=)"
        typ <- getTyp aLhs
        Ok (ETyped (EAss aLhs aRhs) typ)
    ETyped _ typ            -> Ok exp

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
          Bad ("Type mismatch. Exp " ++ printTree exp ++ " should be of type " ++ printTree typ ++ " but has type " ++ printTree isTyp)
    Bad s -> Bad s

-- checks if types of both given expressions are equal, returns annotated expressions or error
checkExpTypeEquality :: Env -> Exp -> Exp -> Err (Exp, Exp)
checkExpTypeEquality env lhs rhs =
  do
    aLhs <- checkExp env lhs
    typLhs <- getTyp aLhs
    aRhs <- checkExp env rhs
    typRhs <- getTyp aRhs
    if (getTyp aLhs == getTyp aRhs) then
      Ok (aLhs, aRhs)
    else
      Bad ("Types of expressions don't match. lhs: " ++ printTree lhs ++ " of type " ++ printTree typLhs ++ " rhs: " ++ printTree rhs ++ " of type " ++ printTree typRhs)

-- infer type of unary operator
checkUnaryOperator :: Env -> Exp -> [Type] -> String -> Err Exp
checkUnaryOperator env exp types name =
  do
    aExp <- checkExp env exp
    typ <- getTyp aExp
    if (elem typ types) then
      Ok aExp
    else
      Bad ("Unary operators " ++ name ++ " is only defined for types " ++ show types)

-- infer type of arithmetic operator
checkBinaryOperator :: Env -> Exp -> Exp -> [Type] -> String -> Err (Exp, Exp)
checkBinaryOperator env lhs rhs types name =
  do
    aExpLhs <- checkExp env lhs
    lhsTyp <- getTyp aExpLhs
    if (elem lhsTyp types) then
      do
        aExpRhs <- checkExpType env rhs lhsTyp
        Ok (aExpLhs, aExpRhs)
    else
      Bad ("Arithmetic operator " ++ name ++ " is only definded for types " ++ show types)

-- Get Type wrapped in ETyped
getTyp :: Exp -> Err Type
getTyp exp =
  case exp of
    ETyped _ typ -> Ok typ
    -- should not be called for other exps'


