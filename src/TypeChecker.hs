module TypeChecker (typecheck) where

import Fox.Abs
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

type VEnv = Map.Map Ident Type
type FEnv = Map.Map Ident (Type, [Type])
type Env = (VEnv, FEnv, Maybe Type)

eqType :: Type -> Type -> Bool
eqType (Int _) (Int _) = True
eqType (Str _) (Str _) = True
eqType (Bool _) (Bool _) = True
eqType _ _ = False

typecheckExpr :: Expr -> ReaderT Env (ExceptT String Identity) Type
typecheckExpr (EVar pos ident) = do
    (venv, fenv, fun) <- ask
    case Map.lookup ident venv of
        Nothing -> throwError $ showIdent ident ++ " not declared."
        Just ttype -> do
            let ttype' = ttype
            return ttype'
typecheckExpr (ELitInt pos int) = do
    return (Int pos)
typecheckExpr (ELitTrue pos) = do
    return (Bool pos)
typecheckExpr (ELitFalse pos) = do
    return (Bool pos)
typecheckExpr (EApp pos ident exprs) = do
    (venv, fenv, fun) <- ask
    case Map.lookup ident fenv of
        Nothing -> throwError $ showIdent ident ++ " not declared."
        Just (ttype, ttypes) -> do
            (if length ttypes == length exprs then (do
                 forM_ (zip ttypes exprs) (uncurry ensureType)
                 return ttype) else throwError "Number of arguments don't match.")
            where
                ensureType ttype' expr = do
                    exprType <- typecheckExpr expr
                    (if eqType ttype' exprType then return ttype else throwError "Types of arguments don't match.")
typecheckExpr (EString pos string) = do
    return (Str pos)
typecheckExpr (Neg pos expr) = do
    exprType <- typecheckExpr expr
    (if eqType exprType (Int pos) then (do
         return exprType) else throwError "Negation on non integer type.")
typecheckExpr (Not pos expr) = do
    exprType <- typecheckExpr expr
    (if eqType exprType (Bool pos) then (do
         return exprType) else throwError "Logical negation on non boolean type.")
typecheckExpr (EMul pos expr1 _ expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    (if eqType expr1Type (Int pos) && eqType expr2Type (Int pos) then (do
         return expr1Type) else throwError "Multiplicative operator on non integer types.")
typecheckExpr (EAdd pos expr1 (Plus _) expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    if eqType expr1Type (Bool pos) || eqType expr2Type (Bool pos)
        then throwError "Add operator on boolean type."
    else if eqType expr1Type expr2Type
            then return expr1Type
            else throwError "Type mismatch."
typecheckExpr (EAdd pos expr1 (Minus _) expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    (if eqType expr1Type (Int pos) && eqType expr2Type (Int pos) then (do
         return expr1Type) else throwError "Minus operator on non integer types.")
typecheckExpr (ERel pos expr1 (LTH pos') expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    if eqType expr1Type (Bool pos) || eqType expr2Type (Bool pos)
        then throwError "Order operator on boolean type."
    else if eqType expr1Type expr2Type
            then return (Bool pos')
            else throwError "Type mismatch."
typecheckExpr (ERel pos expr1 (LE  pos') expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    if eqType expr1Type (Bool pos) || eqType expr2Type (Bool pos)
        then throwError "Order operator on boolean type."
    else if eqType expr1Type expr2Type
            then return (Bool pos')
            else throwError "Type mismatch."
typecheckExpr (ERel pos expr1 (GTH  pos') expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    if eqType expr1Type (Bool pos) || eqType expr2Type (Bool pos)
        then throwError "Order operator on boolean type."
    else if eqType expr1Type expr2Type
            then return (Bool pos')
            else throwError "Type mismatch."
typecheckExpr (ERel pos expr1 (GE  pos') expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    if eqType expr1Type (Bool pos) || eqType expr2Type (Bool pos)
        then throwError "Order operator on boolean type."
    else if eqType expr1Type expr2Type
            then return (Bool pos')
            else throwError "Type mismatch."
typecheckExpr (ERel pos expr1 (EQU pos') expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    if eqType expr1Type expr2Type
        then return (Bool pos')
        else throwError "Type mismatch."
typecheckExpr (ERel pos expr1 (NE pos') expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    if eqType expr1Type expr2Type
        then return (Bool pos')
        else throwError "Type mismatch."
typecheckExpr (EAnd pos expr1 expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    (if eqType expr1Type (Bool pos) && eqType expr2Type (Bool pos) then (do
         return expr1Type) else throwError "&& operator on non boolean types.")
typecheckExpr (EOr pos expr1 expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    (if eqType expr1Type (Bool pos) && eqType expr2Type (Bool pos) then (do
         return expr1Type) else throwError "|| operator on non boolean types.")

showIdent :: Ident -> String
showIdent (Ident str) = str

typecheckStmt :: Stmt -> ReaderT Env (ExceptT String Identity) Env
typecheckStmt (BlStmt pos (Bl pos' stmts)) = do
    env <- ask
    foldM_ applyLocal env stmts
    return env
        where
            applyLocal env' stmt = local (const env') (typecheckStmt stmt)
typecheckStmt (Decl pos ttype ident expr) = do
    (venv, fenv, fun) <- ask
    ttype' <- typecheckExpr expr
    (if eqType ttype' ttype then (do
            let newVenv = Map.insert ident ttype venv
            return (newVenv, fenv, fun)) else throwError "Type mismatch.")
typecheckStmt (Ass pos ident expr) = do
    (venv, fenv, fun) <- ask
    case Map.lookup ident venv of
        Nothing -> throwError $ showIdent ident ++ " not declared."
        Just ttype -> do
            ttype' <- typecheckExpr expr
            (if eqType ttype' ttype then (do
                 return (venv, fenv, fun)) else throwError "Type mismatch.")
typecheckStmt (Ret pos expr) = do
    (venv, fenv, fun) <- ask
    case fun of
        Nothing -> throwError "Return outside of function."
        Just ttype -> do
            ttype' <- typecheckExpr expr
            (if eqType ttype' ttype then (do
                 return (venv, fenv, fun)) else throwError "Type mismatch.")
typecheckStmt (If pos expr stmt) = do
    ttype <- typecheckExpr expr
    case ttype of
        (Bool _) -> typecheckStmt stmt
        _ -> throwError "Type mismatch."
typecheckStmt (IfElse pos expr stmt1 stmt2) = do
    ttype <- typecheckExpr expr
    case ttype of
        (Bool _) -> do
            typecheckStmt stmt1
            typecheckStmt stmt2
        _ -> throwError "Type mismatch."
typecheckStmt (While pos expr stmt) = do
    ttype <- typecheckExpr expr
    case ttype of
        (Bool _) -> do
            typecheckStmt stmt
        _ -> throwError "Type mismatch."
typecheckStmt (FSt pos (FunDef pos' ttype ident args block)) = do
    (venv, fenv, fun) <- ask
    newVenv <- foldM handleArg venv args
    let newFenv = Map.insert ident (ttype, map mapArgToType args) fenv

    local (const (newVenv, newFenv, Just ttype)) (typecheckStmt (BlStmt pos' block))
    return (venv, newFenv, fun)
        where
            mapArgToType (ArgVar _ ttype' _) = ttype'
            mapArgToType (ArgVal _ ttype' _) = ttype'
            handleArg venv' (ArgVar _ ttype' ident') = handleArgGeneric venv' ttype' ident'
            handleArg venv' (ArgVal _ ttype' ident') = handleArgGeneric venv' ttype' ident'

            handleArgGeneric venv' ttype' ident' = do
                let venv'' = Map.insert ident' ttype' venv'
                return venv''

typecheckProgram :: Program -> ReaderT Env (ExceptT String Identity) ()
typecheckProgram (Prog pos stmts) = do
    env <- ask
    foldM_ applyLocal env stmts
        where
            applyLocal env' stmt = local (const env') (typecheckStmt stmt)

typecheck :: Program -> Either String ()
typecheck program = runIdentity (runExceptT (runReaderT (typecheckProgram program) (Map.empty, Map.empty, Nothing)))

