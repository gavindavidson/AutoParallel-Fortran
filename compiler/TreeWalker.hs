

module Main where

import Data.Generics
import Data.Generics.Aliases
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
	let b = map getVariables a
	putStr (show b)
	putStr "\n"

parseTest s = do f <- readFile s
                 return $ parse $ preProcess f


-- Increase salary by percentage
increase :: (Typeable p, Data p) => ProgUnit p -> ProgUnit p
increase = everywhere (mkT incS)

getVariables :: (Typeable p, Data p) => p -> [(Block p)]
getVariables p a = listify (\ ( _ :: (Block a)) -> False) p

-- "interesting" code for increase
incS :: SrcSpan -> SrcSpan
incS (a, b) = (SrcLoc {srcFilename = "test", srcLine = 10, srcColumn = -1}, b)

---- Increase salary by percentage
--increase :: Float -> Company -> Company
--increase k = everywhere (mkT (incS k))

---- "interesting" code for increase
--incS :: Float -> Salary -> Salary
--incS k (S s) = S (s * (1+k))

--data Company  = C [Dept]               deriving (Eq, Show, Typeable, Data)
--data Dept     = D Name Manager [Unit]  deriving (Eq, Show, Typeable, Data)
--data Unit     = PU Employee | DU Dept  deriving (Eq, Show, Typeable, Data)
--data Employee = E Person Salary        deriving (Eq, Show, Typeable, Data)
--data Person   = P Name Address         deriving (Eq, Show, Typeable, Data)
--data Salary   = S Float                deriving (Eq, Show, Typeable, Data)