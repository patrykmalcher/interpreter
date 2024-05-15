import System.Environment
import Fox.Abs
import Fox.ErrM
import Fox.Par
import TypeChecker

parseFile :: String -> IO (Maybe Program)
parseFile input =
    case pProgram (myLexer input) of
        Bad err -> do
            return Nothing
        Ok tree -> do
            return (Just tree)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            input <- readFile fileName
            parsed <- parseFile input
            case parsed of
                Nothing -> putStrLn "Parsing failed."
                Just tree -> do
                    case TypeChecker.typecheck tree of
                        (Left error) -> putStrLn error
                        (Right success) -> putStrLn "SUCCESS"
        _ -> putStrLn "Usage: ./Main <file name>"
