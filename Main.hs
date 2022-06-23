module Main where
-- stack ghc -- Main.hs -o test

import Parser

test e = case e of
        Left err -> putStrLn "[!] Test fail" >> print err
        Right n -> putStrLn "[+] Test pass" >> print n

main :: IO ()
main = do
    test $ parseExpr "1.2"
    test $ parseExpr "1"
    test $ parseExpr "abc"