module Interpreter (interpret) where

import Fox.Abs
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as Map
import qualified Data.Map.Internal.Debug
import System.IO

type Loc = Int
data Value = IntVal Integer | BoolVal Bool | StringVal String

showPos :: BNFC'Position -> String
showPos (Just (line, col)) =
    "line " ++ show line ++ " col "  ++ show col;

instance Eq Value where
    (IntVal x) == (IntVal y) = x == y
    (BoolVal x) == (BoolVal y) = x == y
    (StringVal x) == (StringVal y) = x == y

instance Ord Value where
    (IntVal x) < (IntVal y) = x < y
    (StringVal x) < (StringVal y) = x < y
    (IntVal x) <= (IntVal y) = x <= y
    (StringVal x) <= (StringVal y) = x <= y

instance Show Value where
    show (IntVal x) = show x
    show (BoolVal x) = show x
    show (StringVal x) = show x

negOp :: Value -> Value
negOp (IntVal i) = IntVal (negate i)

notOp :: Value -> Value
notOp (BoolVal b) = BoolVal (not b)

plusOp :: Value -> Value -> Value
plusOp (IntVal x) (IntVal y) = IntVal (x + y)
plusOp (StringVal x) (StringVal y) = StringVal (x ++ y)

minusOp :: Value -> Value -> Value
minusOp (IntVal x) (IntVal y) = IntVal (x - y)

mulOp :: Value -> Value -> Value
mulOp (IntVal x) (IntVal y) = IntVal (x * y)

divideOp :: Value -> Value -> Value
divideOp (IntVal x) (IntVal y) = IntVal (x `div` y)

modOp :: Value -> Value -> Value
modOp (IntVal x) (IntVal y) = IntVal (x `mod` y)

andOp :: Value -> Value -> Value
andOp (BoolVal x) (BoolVal y) = BoolVal (x && y)

orOp :: Value -> Value -> Value
orOp (BoolVal x) (BoolVal y) = BoolVal (x || y)

type Store = (VStore, FStore, Maybe Value)
type VStore = Map.Map Loc Value
type FStore = Map.Map Loc ([Arg], Block, Env)

modifyVStore :: (VStore -> VStore) -> ReaderT Env (StateT Store (ExceptT String IO)) ()
modifyVStore f = modify (\(vstore, fstore, val) -> (f vstore, fstore, val))

modifyFStore :: (FStore -> FStore) -> ReaderT Env (StateT Store (ExceptT String IO)) ()
modifyFStore f = modify (\(vstore, fstore, val) -> (vstore, f fstore, val))

modifyVal :: Maybe Value -> ReaderT Env (StateT Store (ExceptT String IO)) ()
modifyVal newVal = modify (\(vstore, fstore, val) -> (vstore, fstore, newVal))

newLocV :: VStore -> Loc
newLocV store = length (Map.toList store)

newLocF :: FStore -> Loc
newLocF store = length (Map.toList store)

type Env = (Venv, Fenv)
type Venv = Map.Map Ident Loc
type Fenv = Map.Map Ident Loc

interpretExpr :: Expr -> ReaderT Env (StateT Store (ExceptT String IO)) Value
interpretExpr (EVar pos ident) = do
    (venv, _) <- ask
    (vstore, _, _) <- get
    case Map.lookup ident venv of
        Just loc -> do
            case Map.lookup loc vstore of
                Just val -> do
                    return val
interpretExpr (Neg pos expr) = do
    exprVal <- interpretExpr expr
    return (negOp exprVal);
interpretExpr (Not pos expr) = do
    exprVal <- interpretExpr expr
    return (notOp exprVal)
interpretExpr (ELitInt pos int) = do
    return (IntVal int)
interpretExpr (ELitTrue pos) = do
    return (BoolVal True)
interpretExpr (ELitFalse pos) = do
    return (BoolVal False)
interpretExpr (EString pos string) = do
    return (StringVal string)
interpretExpr (EApp pos ident exprs) = do
    (venv, fenv) <- ask
    (_, fstore, _) <- get
    case Map.lookup ident fenv of
        Just loc -> do
            case Map.lookup loc fstore of
                Just (args, block, env) -> do
                    (env', _, _) <- foldM argToDeclExpr (env, (venv, fenv), Nothing) (zip args exprs)
                    env <- local (const env') (interpretStmt (BlStmt pos block))
                    (_, _, val) <- get
                    case val of
                        Just val' -> do
                            modifyVal Nothing
                            return val'
    where
        argToDeclExpr ((venvd, fenvd), (venve, fenve), _) (arg, expr) = do
            case arg of
                (ArgVal _ ttype id) -> do
                    exprVal <- local (const (venve, fenve)) (interpretExpr expr)
                    (vstore, _, _) <- get
                    let newloc = (newLocV vstore)
                    modifyVStore (Map.insert newloc exprVal)
                    return ((Map.insert id newloc venvd, fenvd), (venve, fenve), Nothing)
                (ArgVar _ ttype id) -> do
                    case expr of
                        EVar _ id' -> do
                            case Map.lookup id' venve of
                                Just loc -> do
                                    return ((Map.insert id loc venvd, fenvd), (venve, fenve), Nothing)
interpretExpr (ERel pos expr1 (LTH _) expr2) = do
    expr1Val <- interpretExpr expr1
    expr2Val <- interpretExpr expr2
    return (BoolVal (expr1Val < expr2Val))
interpretExpr (ERel pos expr1 (EQU _) expr2) = do
    expr1Val <- interpretExpr expr1
    expr2Val <- interpretExpr expr2
    return (BoolVal (expr1Val == expr2Val))
interpretExpr (ERel pos expr1 (NE _) expr2) = do
    expr1Val <- interpretExpr expr1
    expr2Val <- interpretExpr expr2
    return (BoolVal (expr1Val /= expr2Val))
interpretExpr (EAdd pos expr1 (Plus _) expr2) = do
    expr1Val <- interpretExpr expr1
    expr2Val <- interpretExpr expr2
    return (plusOp expr1Val expr2Val)
interpretExpr (EAdd pos expr1 (Minus _) expr2) = do
    expr1Val <- interpretExpr expr1
    expr2Val <- interpretExpr expr2
    return (minusOp expr1Val expr2Val)
interpretExpr (EMul pos expr1 (Times _) expr2) = do
    expr1Val <- interpretExpr expr1
    expr2Val <- interpretExpr expr2
    return (mulOp expr1Val expr2Val)
interpretExpr (EMul pos expr1 (Div _) expr2) = do
    expr1Val <- interpretExpr expr1
    expr2Val <- interpretExpr expr2
    case expr2Val of
        IntVal 0 -> throwError $ "Error: division by zero at " ++ showPos pos ++ "."
        _ -> return (divideOp expr1Val expr2Val)
interpretExpr (EMul pos expr1 (Mod _) expr2) = do
    expr1Val <- interpretExpr expr1
    expr2Val <- interpretExpr expr2
    return (modOp expr1Val expr2Val)
interpretExpr (ERel pos expr1 (LE _) expr2) = do
    expr1Val <- interpretExpr expr1
    expr2Val <- interpretExpr expr2
    return (BoolVal (expr1Val <= expr2Val))
interpretExpr (ERel pos expr1 (GTH _) expr2) = do
    expr1Val <- interpretExpr expr1
    expr2Val <- interpretExpr expr2
    return (BoolVal (expr1Val > expr2Val))
interpretExpr (ERel pos expr1 (GE _) expr2) = do
    expr1Val <- interpretExpr expr1
    expr2Val <- interpretExpr expr2
    return (BoolVal (expr1Val >= expr2Val))
interpretExpr (EAnd pos expr1 expr2) = do
    expr1Val <- interpretExpr expr1
    expr2Val <- interpretExpr expr2
    return (andOp expr1Val expr2Val)
interpretExpr (EOr pos expr1 expr2) = do
    expr1Val <- interpretExpr expr1
    expr2Val <- interpretExpr expr2
    return (orOp expr1Val expr2Val)

interpretStmt :: Stmt -> ReaderT Env (StateT Store (ExceptT String IO)) Env
interpretStmt (BlStmt pos (Bl pos' stmts)) = do
    env <- ask
    (_, _, val) <- get
    case val of
        Just val' -> do
            return env
        Nothing -> do
            foldM_ applyLocal env stmts
            return env
    where
        applyLocal env' stmt = local (const env') (interpretStmt stmt)
interpretStmt (Decl pos ttype ident expr) = do
    (venv, fenv) <- ask
    (vstore, fstore, val) <- get
    case val of
        Just _ -> do
            return (venv, fenv)
        Nothing -> do
            let newloc = newLocV vstore
            exprVal <- interpretExpr expr
            modifyVStore (Map.insert newloc exprVal)
            return (Map.insert ident newloc venv, fenv)
interpretStmt (Print pos expr) = do
    env <- ask
    (_, _, val) <- get
    case val of
        Just _ -> do
            return env
        Nothing -> do
            exprVal <- interpretExpr expr
            liftIO $ print exprVal
            return env
interpretStmt (Ass pos ident expr) = do
    (venv, fenv) <- ask
    (_, _, val) <- get
    case val of
        Just _ -> do
            return (venv, fenv)
        Nothing -> do
            exprVal <- interpretExpr expr
            case Map.lookup ident venv of
                Just loc -> do
                    modifyVStore (Map.insert loc exprVal)
                    return (venv, fenv)
interpretStmt (Ret pos expr) = do
    env <- ask
    (_, _, val) <- get
    case val of
        Just _ -> do
            return env
        Nothing -> do
            exprVal <- interpretExpr expr
            modifyVal (Just exprVal)
            return env
interpretStmt (If pos expr block) = do
    env <- ask
    (_, _, val) <- get
    case val of
        Just _ -> do
            return env
        Nothing -> do
            exprVal <- interpretExpr expr
            case exprVal of
                BoolVal bool ->
                    (if bool then (do
                        interpretStmt (BlStmt pos block)) else (do
                        return env))
interpretStmt (IfElse pos expr block1 block2) = do
    env <- ask
    (_, _, val) <- get
    case val of
        Just _ -> do
            return env
        Nothing -> do
            exprVal <- interpretExpr expr
            case exprVal of
                BoolVal bool ->
                    (if bool then (do
                        interpretStmt (BlStmt pos block1)) else (do
                        interpretStmt (BlStmt pos block2)))
interpretStmt (While pos expr block) = do
    env <- ask
    (_, _, val) <- get
    case val of
        Just _ -> do
            return env
        Nothing -> do
            exprVal <- interpretExpr expr
            case exprVal of
                BoolVal bool ->
                    (if bool then (do
                        interpretStmt (BlStmt pos block)
                        interpretStmt (While pos expr block)) else
                        return env)
interpretStmt (FSt pos (FunDef pos' ttype ident args block)) = do
    (venv, fenv) <- ask
    (vstore, fstore, val) <- get
    case val of
        Just _ -> do
            return (venv, fenv)
        Nothing -> do
            let newloc = newLocF fstore
                newfenv = Map.insert ident newloc fenv
            modifyFStore (Map.insert newloc (args, block, (venv, newfenv)))
            return (venv, newfenv)

interpretProgram :: Program -> ReaderT Env (StateT Store (ExceptT String IO)) ()
interpretProgram (Prog pos stmts) = do
    env <- ask
    foldM_ applyLocal env stmts
        where
            applyLocal env' stmt = local (const env') (interpretStmt stmt)

initEnv :: Env
initEnv = (Map.empty, Map.empty)

initStore :: Store
initStore = (Map.empty, Map.empty, Nothing)

interpret :: Program -> IO ()
interpret program = do
    result <- runExceptT (runStateT (runReaderT (interpretProgram program) initEnv) initStore)
    case result of
        Left err -> hPutStrLn stderr err
        Right _ -> return ()