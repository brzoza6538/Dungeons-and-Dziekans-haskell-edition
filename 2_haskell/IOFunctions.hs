module IOFunctions where

import Data.Char (isSpace)
import Data.List (find, intercalate)

printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)


parseCommand :: String -> (String, String)
parseCommand input =
    let (cmd:args) = words input ++ [""]
        trimmedArgs = dropWhile isSpace (reverse (dropWhile isSpace (reverse (unwords args))))
    in (cmd, trimmedArgs)

readCommand :: IO String
readCommand = do
    putStr "> "
    xs <- getLine
    return xs


