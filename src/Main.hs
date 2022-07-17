module Main where

import Control.Monad.Trans
import Parser
import System.Console.Haskeline

test e = case e of
  Left err -> putStrLn "[!] Test fail" >> print err
  Right n -> putStrLn "[+] Test pass" >> print n

testFail e = case e of
  Left err -> putStrLn "[+] Test pass" >> print err
  Right n -> putStrLn "[!] Test fail" >> print n

testAll :: IO ()
testAll = do
  test $ parseExpr "1.2;"
  test $ parseExpr "1;"
  test $ parseExpr "abc;"
  test $ parseExpr "def foo(x y) x+foo(y, 4.0);"
  test $ parseExpr "def foo(x y) x+y; y;"
  test $ parseExpr "extern sin(a);"
  testFail $ parseExpr "def foo(x y) x+y );"

repl :: String -> IO ()
repl line = do
  case parseExpr line of
    Left err -> print err
    Right value -> print value

-- https://hackage.haskell.org/package/haskeline-0.8.2/docs/System-Console-Haskeline.html#v:runInputT
-- https://stackoverflow.com/questions/58366794/what-is-the-purpose-of-liftio
-- not sure the differences between outputStrLn and putStrLn
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