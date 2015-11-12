module Main where

import Language.Fortran.Parser
import Data.Char
import PreParser

main :: IO ()
-- main = return ()
main = do
	a <- parseTest "select_ex.f95"
	putStr $ show a

parseTest s = do f <- readFile s
                 return $ parse $ preParse f
