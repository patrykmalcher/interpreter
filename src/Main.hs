import System.Environment
import Fox.Abs
import Fox.ErrM
import Fox.Par
import TypeChecker
import Interpreter
import System.IO


parseFile :: String -> IO (Maybe Program)
parseFile input =
    case pProgram (myLexer input) of
        Bad err -> do
            return Nothing
        Ok tree -> do
            return (Just tree)

interpretFile :: String -> IO ()
interpretFile input = do
    parsed <- parseFile input
    case parsed of
        Nothing -> hPutStrLn stderr "Parsing failed - not valid .fox program."
        Just tree -> do
            case TypeChecker.typecheck tree of
                (Left error) -> hPutStrLn stderr error
                (Right _) -> Interpreter.interpret tree

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            input <- readFile fileName
            interpretFile input
        [] -> do
            input <- getContents
            interpretFile input
        _ -> putStrLn "Usage: ./Main <file name>"
