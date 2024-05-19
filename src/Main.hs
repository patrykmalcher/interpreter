import System.Environment
import Fox.Abs
import Fox.ErrM
import Fox.Par
import TypeChecker
import Interpreter
import System.IO
import System.Exit (exitWith, ExitCode(ExitFailure))


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
        Nothing -> do 
            hPutStrLn stderr "Parsing failed - not valid .fox program."
            exitWith (ExitFailure 1)
        Just tree -> do
            case TypeChecker.typecheck tree of
                (Left error) -> do 
                    hPutStrLn stderr error
                    exitWith (ExitFailure 1)
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
