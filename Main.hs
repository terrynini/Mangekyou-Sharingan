module Main where
-- stack ghc -- Main.hs -o test

import Parser
import Control.Monad.Trans
import System.Console.Haskeline
import Control.Arrow (ArrowLoop(loop))

test e = case e of
        Left err -> putStrLn "[!] Test fail" >> print err
        Right n -> putStrLn "[+] Test pass" >> print n

testAll :: IO ()
testAll = do
    test $ parseExpr "1.2"
    test $ parseExpr "1"
    test $ parseExpr "abc"

repl :: String -> IO ()
repl line = do
            case parseExpr line of
                Left err -> print err
                Right value -> print value

-- https://hackage.haskell.org/package/haskeline-0.8.2/docs/System-Console-Haskeline.html#v:runInputT
-- https://stackoverflow.com/questions/58366794/what-is-the-purpose-of-liftio
-- not sure the different between outputStrLn and putStrLn
main :: IO ()
main = do
    testAll
    runInputT defaultSettings loop
    where
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "=>"
            case minput of
                Nothing -> outputStrLn "Bye!"
                Just input -> liftIO (repl input) >> loop
