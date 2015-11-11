module Main where

import Language.Fortran.Parser

main :: IO ()
-- main = return ()
main = do
	a <- parseTest "if_ex.f95"
	putStr $ show a

parseTest s = do f <- readFile s
                 return $ parse f
