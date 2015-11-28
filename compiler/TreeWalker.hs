{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

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


-- suggestion: foo = (\(@ iType) _ -> undefined @ iType, HiddenType...)

	--module Foo where
	--import Data.Proxy
	--import Data.Typeable
	--data HiddenType where
	--Hidden :: (Typeable a) => Proxy a -> HiddenType
	--foo :: (i,HiddenType)
	--foo = (undefined, Hidden (Proxy::Proxy Int))
	--data Foo where
	--Foo :: i -> Foo
	--bar :: Foo
	--bar =
	--let (x,h) = foo
	--in case h of
	--(Hidden (p::Proxy i)) -> Foo (x :: i)

main :: IO ()
-- main = return ()
main = do
	a <- parseTest "../testFiles/arrayLoop.f95"
	--f <- readFile "continuation.f95"
	--let a = preProcess f
	--let loops = identifyLoops (a!!0)
	--putStr (show (loops!!0))
	let firstProg = increase (a!!0)
	--putStr (show (paralleliseLoop (loops!!0)))
	putStr "\n"
	putStr $ show $ (a!!0)
	putStr "\n"
	putStr $ show $ firstProg
	--putStr (show a)

	--let edit = increase (a!!0)
	--putStr "\n"
	--putStr (show edit)

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

--editFunction :: Data p => [ProgUnit p] -> ProgUnit p
--editFunction a = increase (a!!0)

paralleliseLoop :: (Typeable p, Data p) => Fortran p -> Fortran p
paralleliseLoop loop 	| parallelisableLoop_map loop = arbitraryChange loop
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
--increase :: (Typeable p, Data p) => ProgUnit p -> ProgUnit p
increase :: (Data a, Typeable a) => ProgUnit a -> ProgUnit a
increase = everywhere (mkT (forEdit)) 

arbitraryChange :: (Data a, Typeable a) => Fortran a -> Fortran a
arbitraryChange = everywhere (mkT (incS)) 

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
incS (a, b) = (SrcLoc {srcFilename = "CHANGE", srcLine = 10, srcColumn = -1}, b)

forEdit ::  Fortran () -> Fortran ()
forEdit inp = case inp of
		For _ _ _ _ _ _ _ -> paralleliseLoop inp
		_ -> inp
--forEdit (UseBlock a b) = UseBlock a b

--forEdit (Main p srcspan subname arg block progs) = (Main p srcspan subname arg block progs) 


-- Names declared in an equation
--decsEqua :: Equation -> [Name]
--decsEqua (E ps _ _) = everything union ([] `mkQ` pvar) ps
--  where
--    pvar (PVar n) = [n]
--    pvar _        = []