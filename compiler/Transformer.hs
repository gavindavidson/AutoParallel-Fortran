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
	--putStr "\n\n\n"
	--putStr $ show $ parallelisedProg
	--putStr "\n"

	let loops = identifyLoops (a!!0)
	let test = Prelude.map (paralleliseLoop []) loops
	--let gmapTest = gmapQ (mkQ True (checkAssignments [])) (loops!!0)
	--putStr $ show $ gmapTest
	--putStr "\n"
	--let mkQ_test = gmapQ getSrcSpan ((loops!!0))
	--putStr $ show $ mkQ_test
	let varname_test = getVarNames_tst (loops!!0)
	putStr $ show $ varname_test
	putStr "\n"

parseTest s = do f <- readFile s
                 return $ parse $ preProcess f


paralleliseLoop :: (Typeable p, Data p, Eq p) => [(VarName p)] -> Fortran p -> Fortran p
paralleliseLoop loopVars loop = case paralleliseLoop_map loop newLoopVars of 
									Just a -> a
									Nothing -> loop
								where
									newLoopVars = case getLoopVar loop of
										Just a -> loopVars ++ [a]
										Nothing -> loopVars

paralleliseLoop_d :: (Typeable p, Data p, Eq p) => [(VarName p)] -> Fortran p -> [(VarName p)]
paralleliseLoop_d loopVars loop = newLoopVars
								where
									newLoopVars = case getLoopVar loop of
										Just a -> loopVars ++ [a]
										Nothing -> loopVars

paralleliseLoop_map ::(Typeable p, Data p, Eq p) => Fortran p -> [(VarName p)] -> Maybe (Fortran p)
paralleliseLoop_map loop loopVars	|	all (== True) assignmentsCheck = Just (arbitraryChange_allChildren "PARALLEL" loop)
									|	otherwise		= Nothing
	where
		assignmentsCheck = Prelude.map (checkArrayAssignments loopVars) assignments
		assignments = getAssigments loop

parallelisableLoop_reduce ::(Typeable p, Data p) => Fortran p -> Bool
parallelisableLoop_reduce loop = False

paralleliseProgram :: (Typeable p, Data p) => ProgUnit p -> ProgUnit p 
paralleliseProgram = everywhere (mkT (forTransform))

-- (Typeable p, Data p, Ord p)
identifyLoops :: (Typeable p, Data p) => ProgUnit p -> [Fortran ()]
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

getVarNames :: (Typeable p, Data p) =>  [Expr p] -> [VarName p]
getVarNames exp = everything (++) (mkQ [] checkForVarName) exp

getVarNames_tst :: (Typeable p, Data p) =>  Fortran p -> [VarName p]
getVarNames_tst exp = everything (++) (mkQ [] checkForVarName) exp


	-- everything (++) (mkQ empty checkForAssignment) loop

checkForAssignment :: (Typeable p, Data p) => Fortran p -> [Fortran p]
checkForAssignment codeSeg = case codeSeg of
		Assg _ _ _ _ -> [codeSeg]
		_ -> []

getSrcSpan :: Data a => a -> SrcSpan
getSrcSpan codeSeg = (SrcLoc {srcFilename = "comment", srcLine = 10, srcColumn = -1}, 
							SrcLoc {srcFilename = "comment", srcLine = 10, srcColumn = -1})

checkForVarName :: (Typeable p, Data p) =>  VarName p -> [VarName p]
checkForVarName exp = [exp]

--checkAssignment :: (Typeable p, Data p) => Fortran p -> Bool
--checkAssignment codeSeg = case codeSeg of
--		Assg _ _ _ _ -> True
--		_ -> []

checkAssignments :: [VarName ()] -> Fortran () -> Bool
checkAssignments loopVars codeSeg = case codeSeg of
		Assg _ _ (Var _ _ lst) expr2 -> contains_list loopVars (foldl (getArrayElementVariables_foldl) [] lst)
		For _ _ var _ _ _ _ -> all (== True) (gmapQ (mkQ True (checkAssignments (loopVars ++ [var]))) codeSeg)
		_ -> all (== True) (gmapQ (mkQ True (checkAssignments loopVars)) codeSeg)

checkArrayAssignments :: (Typeable p, Data p, Eq p) => [VarName p] -> Fortran p -> Bool
checkArrayAssignments loopVars assignment = case assignment of
		Assg _ _ (Var _ _ lst) expr2 -> contains_list loopVars (foldl (getArrayElementVariables_foldl) [] lst)

		-- all (== True) (Prelude.map (contains_list loopVars) (getVarNames lst))

		--case lst!!0 of
		--						((VarName _ _), []) -> False
		--						_ -> True
		_	-> False

-- foldl :: (a -> b -> a) -> a -> [b] -> a
--getArrayElementVariables_foldl :: [Expr p] -> (VarName p, [Expr p]) -> [Expr p]
--getArrayElementVariables_foldl prevList (varname, xs) = 

getArrayElementVariables_foldl :: (Typeable p, Data p) => [VarName p] -> (VarName p, [Expr p]) -> [VarName p]
getArrayElementVariables_foldl prev (var, exps) = prev ++ (getVarNames exps)

--getArrayElementVariables :: (VarName p, [Expr p]) -> [VarName p]
--getArrayElementVariables inp = []

getLoopVar :: Fortran p -> Maybe(VarName p)
getLoopVar (For _ _ var _ _ _ _) = Just var
getLoopVar _ = Nothing

contains_list :: Eq p => [VarName p] -> [VarName p] -> Bool
contains_list container contained = all (== True) (Prelude.map (\x -> elem x container) contained)

forTransform :: Fortran () -> Fortran ()
forTransform inp = case inp of
		For _ _ _ _ _ _ _ -> paralleliseLoop [] inp
		_ -> inp

arbitraryChange_allChildren :: (Data a, Typeable a) => String -> Fortran a -> Fortran a
arbitraryChange_allChildren comment = everywhere (mkT (modifySrcSpan_allChildren comment)) 

modifySrcSpan_allChildren :: String -> SrcSpan -> SrcSpan
modifySrcSpan_allChildren comment (a, b) = (SrcLoc {srcFilename = comment, srcLine = 10, srcColumn = -1}, b)