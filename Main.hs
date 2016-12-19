module Main where 
import Scheme
import System.Environment

main = do
    [filePath] <- getArgs
    contents <- readFile filePath
    putStrLn $ show (run contents)