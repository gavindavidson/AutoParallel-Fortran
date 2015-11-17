module Main where

import Language.Fortran.Parser
import Data.Char
import PreProcessor

main :: IO ()
-- main = return ()
main = do
	--a <- parseTest "continuation.f95"
	a <- parseTest "../../F95Sources/timseris.f95"
	--f <- readFile "continuation.f95"
	--let a = preProcess f
	putStr (show a)

parseTest s = do f <- readFile s
                 return $ parse $ preProcess f
