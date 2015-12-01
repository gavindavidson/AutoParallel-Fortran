{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module Main where

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
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
	--let parallelisedProg = paralleliseProgram (a!!0)
	--putStr "\n"
	--putStr $ show $ (a!!0)
	--putStr "\n\n"
	--putStr $ show $ parallelisedProg
	--putStr "\n"

	let loops = identifyLoops (a!!0)
	let test = Prelude.map getAssigments_scoped loops
	putStr $ show $ test
	putStr "\n"

parseTest s = do f <- readFile s
                 return $ parse $ preProcess f


paralleliseLoop :: (Typeable p, Data p) => Fortran p -> [(VarName p)] -> Fortran p
paralleliseLoop loop loopVars  = case paralleliseLoop_map loop loopVars of 
									Just a -> a
									Nothing -> loop

paralleliseLoop_map ::(Typeable p, Data p) => Fortran p -> [(VarName p)] -> Maybe (Fortran p)
paralleliseLoop_map loop loopVars	|	all (== True) assignmentsCheck = Just (arbitraryChange_allChildren "comment" loop)
									|	otherwise		= Nothing
	where
		assignmentsCheck = Prelude.map (checkArrayAssignments loopVars) assignments
		assignments = getAssigments loop

parallelisableLoop_reduce ::(Typeable p, Data p) => Fortran p -> Bool
parallelisableLoop_reduce loop = False

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

getAssigments_scoped :: (Typeable p, Data p) =>  Fortran p -> [Bool]
getAssigments_scoped loop = gmapQ (dummy) loop

dummy :: Data d => d -> Bool
dummy inp = True

getAssigments :: (Typeable p, Data p) =>  Fortran p -> [Fortran p] -- [Fortran p]
getAssigments loop = everything (++) (mkQ [] checkForAssignment) loop

	-- everything (++) (mkQ empty checkForAssignment) loop

checkForAssignment :: (Typeable p, Data p) => Fortran p -> [Fortran p]
checkForAssignment codeSeg = case codeSeg of
		Assg _ _ _ _ -> [codeSeg]
		_ -> []

--checkAssignment :: (Typeable p, Data p) => Fortran p -> Bool
--checkAssignment codeSeg = case codeSeg of
--		Assg _ _ _ _ -> True
--		_ -> []


checkArrayAssignments :: (Typeable p, Data p) => [VarName p] -> Fortran p -> Bool
checkArrayAssignments loopVars assignment = case assignment of
		Assg _ _ (Var _ _ lst) expr2 -> all (== True) (Prelude.map (contains_list loopVars) (Prelude.map getArrayElementVariables lst))

		--case lst!!0 of
		--						((VarName _ _), []) -> False
		--						_ -> True
		_	-> False

getArrayElementVariables :: (VarName p, [Expr p]) -> [Expr p]
getArrayElementVariables inp = []

contains_list :: [p] -> [q] -> Bool
contains_list a b = False

forTransform :: Fortran () -> Fortran ()
forTransform inp = case inp of
		For _ _ _ _ _ _ _ -> paralleliseLoop inp []
		_ -> inp

arbitraryChange_allChildren :: (Data a, Typeable a) => String -> Fortran a -> Fortran a
arbitraryChange_allChildren comment = everywhere (mkT (modifySrcSpan_allChildren comment)) 

modifySrcSpan_allChildren :: String -> SrcSpan -> SrcSpan
modifySrcSpan_allChildren comment (a, b) = (SrcLoc {srcFilename = comment, srcLine = 10, srcColumn = -1}, b)