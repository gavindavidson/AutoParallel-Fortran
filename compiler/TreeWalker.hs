{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Generics (Data, Typeable, mkQ, mkT, everything, everywhere)
import Data.Generics.Aliases
import Data.Generics.Builders
--import Data.List
--import Data.Set
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import PreProcessor


main :: IO ()
-- main = return ()
main = do
	a <- parseTest "../testFiles/arrayLoop.f95"
	--f <- readFile "continuation.f95"
	--let a = preProcess f
	--let b = Prelude.map getVariables a
	putStr (show a)
	putStr "\n"

parseTest s = do f <- readFile s
                 return $ parse $ preProcess f


-- Increase salary by percentage
increase :: (Typeable p, Data p) => ProgUnit p -> ProgUnit p
increase = everywhere (mkT incS)

getVariables :: (Typeable p, Data p, Ord p) => ProgUnit p -> [VarName p]
getVariables inp =
	everything
		(++)
		(mkQ empty (\variable@(VarName _ _) -> [variable]))
		--(mkQ empty (\loop@(For a b c d e f g) -> [loop]))
		inp

--getVars' :: Data d => d -> Set Var
--getVars' code =
--    everything
--        union
--        (mkQ empty (\var@(Var _) -> singleton var))
--        code

-- "interesting" code for increase
incS :: SrcSpan -> SrcSpan
incS (a, b) = (SrcLoc {srcFilename = "test", srcLine = 10, srcColumn = -1}, b)

-- Names declared in an equation
--decsEqua :: Equation -> [Name]
--decsEqua (E ps _ _) = everything union ([] `mkQ` pvar) ps
--  where
--    pvar (PVar n) = [n]
--    pvar _        = []

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