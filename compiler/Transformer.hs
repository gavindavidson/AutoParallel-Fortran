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

main :: IO ()
-- main = return ()
main = do
	a <- parseTest "../testFiles/arrayLoop.f95"
	let parallelisedProg = paralleliseProgram (a!!0)
	--putStr (show (paralleliseLoop (loops!!0)))
	putStr "\n"
	putStr $ show $ (a!!0)
	putStr "\n\n"
	putStr $ show $ parallelisedProg
	putStr "\n"

parseTest s = do f <- readFile s
                 return $ parse $ preProcess f


paralleliseLoop :: (Typeable p, Data p) => Fortran p -> Fortran p
paralleliseLoop loop 	| parallelisableLoop_map loop = arbitraryChange loop
						|	otherwise = loop

parallelisableLoop_map ::(Typeable p, Data p) => Fortran p -> Bool
parallelisableLoop_map loop = all (== True) assignmentsCheck
	where
		assignmentsCheck = Prelude.map checkArrayAssignments assignments
		assignments = getAssigments loop

paralleliseProgram :: (Typeable p, Data p) => ProgUnit p -> ProgUnit p 
paralleliseProgram = everywhere (mkT (forTransform))

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

checkArrayAssignments :: (Typeable p, Data p) => Fortran p -> Bool
checkArrayAssignments assignment = case assignment of
		Assg _ _ (Var _ _ lst) expr2 -> case lst!!0 of
								((VarName _ _), []) -> False
								_ -> True
		_	-> False

forTransform ::  Fortran () -> Fortran ()
forTransform inp = case inp of
		For _ _ _ _ _ _ _ -> paralleliseLoop inp
		_ -> inp

arbitraryChange :: (Data a, Typeable a) => Fortran a -> Fortran a
arbitraryChange = everywhere (mkT (modifySrcSpan)) 

modifySrcSpan :: SrcSpan -> SrcSpan
modifySrcSpan (a, b) = (SrcLoc {srcFilename = "CHANGE", srcLine = 10, srcColumn = -1}, b)