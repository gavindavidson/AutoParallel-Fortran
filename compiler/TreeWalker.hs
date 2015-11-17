module Main where

import Data.Generics
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import PreProcessor


main :: IO ()
-- main = return ()
main = do
	a <- parseTest "../language-fortran/src/if_ex.f95"
	--f <- readFile "continuation.f95"
	--let a = preProcess f
	putStr (show a)

parseTest s = do f <- readFile s
                 return $ parse $ preProcess f

getFortran :: Data a => a -> Fortran p
getFortran t = getFortran_default 	'extQ getFortran_ProgUnit

getFortran_ProgUnit :: ProgUnit p -> Fortran p
getFortran_ProgUnit progUnit = case progUnit of
								Main _ _ _ block _ prog_list -> getFortran block

getFortran_default :: Data a => a -> Fortran p
getFortran_default input_data = case input_data of
								Block _ _ _ _ _ fortran -> fortran