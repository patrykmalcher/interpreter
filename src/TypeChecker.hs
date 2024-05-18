module TypeChecker (typecheck) where

import Fox.Abs
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Set as Set
import Data.Set (Set)

type VEnv = Map.Map Ident Type
type FEnv = Map.Map Ident (Type, [Type])
type Env = (VEnv, FEnv, Maybe Type, Bool)

showPos :: BNFC'Position -> String
showPos (Just (line, col)) =
    "line " ++ show line ++ " col "  ++ show col;

showIdent :: Ident -> String
showIdent (Ident id) = show id

showType :: Type -> String
showType (Int _) = "int"
showType (Str _) = "string"
showType (Bool _) = "boolean"

eqType :: Type -> Type -> Bool
eqType (Int _) (Int _) = True
eqType (Str _) (Str _) = True
eqType (Bool _) (Bool _) = True
eqType _ _ = False

typecheckExpr :: Expr -> ReaderT Env (ExceptT String Identity) Type
typecheckExpr (EVar pos ident) = do
    (venv, fenv, fun, _) <- ask
    case Map.lookup ident venv of
        Nothing -> throwError $ "Error in evaluating variable value at " ++ showPos pos ++ ". " ++ showIdent ident ++ " is not declared."
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
    (venv, fenv, fun, _) <- ask
    case Map.lookup ident fenv of
        Nothing -> throwError $ "Error in function function call at: " ++ showPos pos ++  ". " ++ showIdent ident ++ " is not declared."
        Just (ttype, ttypes) -> do
            (if length ttypes == length exprs then (do
                 forM_ (zip ttypes exprs) (uncurry ensureType)
                 return ttype) else throwError $ "Error in function function call at: " ++ showPos pos ++ ". Number of arguments don't match.")
            where
                ensureType ttype' expr = do
                    exprType <- typecheckExpr expr
                    (if eqType ttype' exprType then return ttype else throwError $ "Error in function function call at: " ++ showPos pos ++ ". Types of arguments don't match.")
typecheckExpr (EString pos string) = do
    return (Str pos)
typecheckExpr (Neg pos expr) = do
    exprType <- typecheckExpr expr
    (if eqType exprType (Int pos) then (do
         return exprType) else throwError $ "Error in operator at " ++ showPos pos ++ ". Negotion was applied to non integer type.")
typecheckExpr (Not pos expr) = do
    exprType <- typecheckExpr expr
    (if eqType exprType (Bool pos) then (do
         return exprType) else throwError $ "Error in operator at " ++ showPos pos ++ ". Logical negotion was applied to non boolean type.")
typecheckExpr (EMul pos expr1 _ expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    (if eqType expr1Type (Int pos) && eqType expr2Type (Int pos) then (do
         return expr1Type) else throwError $ "Error in operator at " ++ showPos pos ++ ". Multiplicative operator was applied to non integer type.")
typecheckExpr (EAdd pos expr1 (Plus _) expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    if eqType expr1Type (Bool pos) || eqType expr2Type (Bool pos)
        then throwError $ "Error in operator at " ++ showPos pos ++ ". + was applied to boolean type."
    else if eqType expr1Type expr2Type
            then return expr1Type
            else throwError  $ "Type mismatch in + operator at " ++ showPos pos ++ ". Left side of expression was type " ++ showType expr1Type ++ ", but right was type " ++ showType expr2Type ++ "."
typecheckExpr (EAdd pos expr1 (Minus _) expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    (if eqType expr1Type (Int pos) && eqType expr2Type (Int pos) then (do
         return expr1Type) else throwError $ "Error in operator at " ++ showPos pos ++ ". - was applied to non integer type.")
typecheckExpr (ERel pos expr1 (EQU pos') expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    if eqType expr1Type expr2Type
        then return (Bool pos')
        else throwError $ "Type mismatch in == operator at " ++ showPos pos ++ ". Left side of expression was type " ++ showType expr1Type ++ ", but right was type " ++ showType expr2Type ++ "."
typecheckExpr (ERel pos expr1 (NE pos') expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    if eqType expr1Type expr2Type
        then return (Bool pos')
        else throwError $ "Type mismatch in != operator at " ++ showPos pos ++ ". Left side of expression was type " ++ showType expr1Type ++ ", but right was type " ++ showType expr2Type ++ "."
typecheckExpr (ERel pos expr1 _ expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    if eqType expr1Type (Bool pos) || eqType expr2Type (Bool pos)
        then throwError $ "Error in operator at " ++ showPos pos ++ ". Order operator was applied to boolean type."
    else if eqType expr1Type expr2Type
            then return (Bool pos)
            else throwError $ "Type mismatch in order opearator at " ++ showPos pos ++ ". Left side of expression was type " ++ showType expr1Type ++ ", but right was type " ++ showType expr2Type ++ "."
typecheckExpr (EAnd pos expr1 expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    (if eqType expr1Type (Bool pos) && eqType expr2Type (Bool pos) then (do
         return expr1Type) else throwError $ "Error in boolean operator at " ++ showPos pos ++ ". && operator on non boolean types.")
typecheckExpr (EOr pos expr1 expr2) = do
    expr1Type <- typecheckExpr expr1
    expr2Type <- typecheckExpr expr2
    (if eqType expr1Type (Bool pos) && eqType expr2Type (Bool pos) then (do
         return expr1Type) else throwError $ "Error in boolean operator at " ++ showPos pos ++ ". || operator on non boolean types.")

typecheckStmt :: Stmt -> ReaderT Env (ExceptT String Identity) Env
typecheckStmt (BlStmt pos (Bl pos' stmts)) = do
    (venv, fenv, fun, blank) <- ask
    (_, _, _, wasReturn) <- foldM applyLocal (venv, fenv, fun, blank) stmts
    return (venv, fenv, fun, wasReturn)
        where
            applyLocal env' stmt = local (const env') (typecheckStmt stmt)
typecheckStmt (Decl pos ttype ident expr) = do
    (venv, fenv, fun, wasReturn) <- ask
    ttype' <- typecheckExpr expr
    (if eqType ttype' ttype then (do
            let newVenv = Map.insert ident ttype venv
            return (newVenv, fenv, fun, wasReturn)) else throwError $ "Type mismatch in declaration at " ++ showPos pos ++ ". " ++ showIdent ident ++ " was declared as " ++ showType ttype ++ " but expression has type " ++ showType ttype' ++ ".")
typecheckStmt (Ass pos ident expr) = do
    (venv, fenv, fun, wasReturn) <- ask
    case Map.lookup ident venv of
        Nothing -> throwError $ "Error in assignment at " ++ showPos pos ++ ". " ++ showIdent ident ++ " is not declared."
        Just ttype -> do
            ttype' <- typecheckExpr expr
            (if eqType ttype' ttype then (do
                 return (venv, fenv, fun, wasReturn)) else throwError $ "Type mismatch in assignment at " ++ showPos pos ++ ". " ++ showIdent ident ++ " has type " ++ showType ttype ++ " but expression has type " ++ showType ttype' ++ ".")
typecheckStmt (Ret pos expr) = do
    (venv, fenv, fun, _) <- ask
    case fun of
        Nothing -> throwError $ "Error in return at " ++ showPos pos ++ ". " ++ "Return outside of the function."
        Just ttype -> do
            ttype' <- typecheckExpr expr
            (if eqType ttype' ttype then (do
                 return (venv, fenv, fun, True)) else throwError $ "Type mismatch in return at " ++ showPos pos ++ ". " ++ "Function was declared " ++ showType ttype ++ " but expression has type " ++ showType ttype' ++ ".")
typecheckStmt (If pos expr block) = do
    env <- ask
    ttype <- typecheckExpr expr
    case ttype of
        (Bool _) -> do 
            typecheckStmt (BlStmt pos block)
            return env
        _ -> throwError $ "Type mismatch in if at " ++ showPos pos ++ ". " ++ "Expression needs to be boolean, but was: " ++ showType ttype ++ "."
typecheckStmt (IfElse pos expr block1 block2) = do
    (venv, fenv, fun, wasReturn) <- ask
    ttype <- typecheckExpr expr
    case ttype of
        (Bool _) -> do
            (_, _, _, wasReturn1) <- typecheckStmt (BlStmt pos block1)
            (_, _, _, wasReturn2) <- typecheckStmt (BlStmt pos block2)
            return (venv, fenv, fun, wasReturn || (wasReturn1 && wasReturn2))
        _ -> throwError $ "Type mismatch in ifelse at " ++ showPos pos ++ ". " ++ "Expression needs to be boolean, but was: " ++ showType ttype ++ "."
typecheckStmt (While pos expr block) = do
    env <- ask
    ttype <- typecheckExpr expr
    case ttype of
        (Bool _) -> do
            typecheckStmt (BlStmt pos block)
            return env
        _ -> throwError $ "Type mismatch in while at " ++ showPos pos ++ ". " ++ "Expression needs to be boolean, but was: " ++ showType ttype ++ "."
typecheckStmt (FSt pos (FunDef pos' ttype ident args block)) = do
    (venv, fenv, fun, _) <- ask
    assureUniqueIdents args Set.empty
    newVenv <- foldM handleArg venv args
    let newFenv = Map.insert ident (ttype, map mapArgToType args) fenv

    (_, _, _, wasReturn) <- local (const (newVenv, newFenv, Just ttype, False)) (typecheckStmt (BlStmt pos' block))
    (if wasReturn then return (venv, newFenv, fun, False) else throwError $ "Error in function declaration at: " ++ showPos pos ++ ". No return statement.")
        where
            assureUniqueIdents :: [Arg] -> Set.Set Ident -> ReaderT Env (ExceptT String Identity) ()
            assureUniqueIdents [] seen = return ()
            assureUniqueIdents ((ArgVal _ ttype' ident'):xs) seen = do
                (if ident' `Set.member` seen then throwError $ "Error in function declaration at: " ++ showPos pos ++ ". Arguments identifiers aren't unique."
                    else assureUniqueIdents xs (Set.insert ident' seen))
            assureUniqueIdents ((ArgVar _ ttype' ident'):xs) seen = do
                (if ident' `Set.member` seen then throwError $ "Error in function declaration at: " ++ showPos pos ++ ". Arguments identifiers aren't unique."
                    else assureUniqueIdents xs (Set.insert ident' seen))

            mapArgToType (ArgVar _ ttype' _) = ttype'
            mapArgToType (ArgVal _ ttype' _) = ttype'
            handleArg venv' (ArgVar _ ttype' ident') = handleArgGeneric venv' ttype' ident'
            handleArg venv' (ArgVal _ ttype' ident') = handleArgGeneric venv' ttype' ident'

            handleArgGeneric venv' ttype' ident' = do
                let venv'' = Map.insert ident' ttype' venv'
                return venv''
typecheckStmt (Print pos expr) = do
    env <- ask
    typecheckExpr expr
    return env

typecheckProgram :: Program -> ReaderT Env (ExceptT String Identity) ()
typecheckProgram (Prog pos stmts) = do
    env <- ask
    foldM_ applyLocal env stmts
        where
            applyLocal env' stmt = local (const env') (typecheckStmt stmt)

typecheck :: Program -> Either String ()
typecheck program = runIdentity (runExceptT (runReaderT (typecheckProgram program) (Map.empty, Map.empty, Nothing, False)))
