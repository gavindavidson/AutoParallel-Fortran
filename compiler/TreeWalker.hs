{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Generics (Data, Typeable, mkQ, mkT, everything, everywhere)
--import Data.Generics.Aliases
--import Data.Generics.Builders
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
	--let loops = identifyLoops (a!!0)
	--putStr (show (loops!!0))
	--putStr "\n"
	--putStr (show (paralleliseLoop (loops!!0)))
	--putStr "\n"
	--putStr (show a)

	let edit = increase (a!!0)
	putStr "\n"
	putStr (show edit)
	putStr "\n"

	--let assignments = Prelude.map getAssigments loops
	--let assignmentsCheck = Prelude.map checkAssignments (assignments!!0)
	--putStr (show (assignments!!0))
	--putStr "\n"
	--putStr (show assignmentsCheck)
	--putStr "\n"
	--putStr (show $ all (== True) assignmentsCheck)
	--putStr "\n"

parseTest s = do f <- readFile s
                 return $ parse $ preProcess f

paralleliseLoop :: (Typeable p, Data p) => Fortran p -> Fortran p
paralleliseLoop loop 	| parallelisableLoop_map loop = loop
						|	otherwise = loop

--parallelisableLoop_map ::(Typeable p, Data p) => Fortran p -> Bool
parallelisableLoop_map loop = all (== True) assignmentsCheck
	where
		assignmentsCheck = Prelude.map checkAssignments assignments
		assignments = getAssigments loop


-- (Typeable p, Data p, Ord p)
identifyLoops :: (Typeable p, Data p) => ProgUnit p -> [Fortran p]
identifyLoops program =
	everything
		(++)
		(mkQ [] checkLoop)
		program

checkLoop inp = case inp of
		For _ _ _ _ _ _ _ -> [inp]
		_ -> []

getAssigments :: (Typeable p, Data p) =>  Fortran p -> [Fortran p] -- [Fortran p]
getAssigments loop = everything (++) (mkQ [] checkForAssignment) loop

	-- everything (++) (mkQ empty checkForAssignment) loop

--checkForAssignment :: (Typeable p, Data p) => Fortran p -> [Fortran p]
checkForAssignment codeSeg = case codeSeg of
		Assg _ _ _ _ -> [codeSeg]
		_ -> []

checkAssignments :: (Typeable p, Data p) => Fortran p -> Bool
checkAssignments assignment = case assignment of
		Assg _ _ (Var _ _ lst) expr2 -> case lst!!0 of
								((VarName _ _), []) -> False
								_ -> True
		_	-> False

--checkAssignments :: (Typeable p, Data p) => Bool -> Fortran p -> Bool
--checkAssignments accum assignment = case assignment of
--		Assg _ _ (Var _ _ lst) expr2 -> case lst!!0 of
--								((VarName _ _), []) -> accum
--								_ -> False
--		_	-> False


-- Increase salary by percentage
increase ::ProgUnit p -> ProgUnit p
increase = everywhere (mkT forEdit)

--getVariables :: (Typeable p, Data p, Ord p) => ProgUnit p -> [VarName p]
--getVariables inp =
--	everything
--		(++)
--		(mkQ empty (\variable@(VarName _ _) -> [variable]))
--		--(mkQ empty (\loop@(For a b c d e f g) -> [loop]))
--		inp

--getVars' :: Data d => d -> Set Var
--getVars' code =
--    everything
--        union
--        (mkQ empty (\var@(Var _) -> singleton var))
--        code

-- "interesting" code for increase
incS :: SrcSpan -> SrcSpan
incS (a, b) = (SrcLoc {srcFilename = "test", srcLine = 10, srcColumn = -1}, b)

forEdit :: Arg p -> Arg p
forEdit something = something 


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