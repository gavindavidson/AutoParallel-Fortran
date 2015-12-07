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
	let parallelisedProg = paralleliseProgram (a!!0)
	--putStr "\n"
	--putStr $ show $ (a!!0)
	--putStr "\n\n\n"
	putStr $ show $ parallelisedProg
	putStr "\n"

	--let loops = identifyLoops (a!!0)
	--let test = Prelude.map (paralleliseLoop []) loops
	--let gmapTest = gmapQ (mkQ True (checkAssignments_map [])) (loops!!0)
	--putStr $ show $ gmapTest
	--putStr "\n"
	--let mkQ_test = gmapQ getSrcSpan ((loops!!0))
	--putStr $ show $ mkQ_test
	--let varname_test = getVarNames_tst (loops!!0)
	--let cmp_first = Prelude.map (== (varname_test!!0)) varname_test
	--putStr $ show $ varname_test
	--putStr "\n"
	----putStr $ show $ cmp_first	
	----putStr "\n"
	--let test = arrayElemTest (loops!!0)
	--putStr $ show $ test
	--putStr "\n"

	--let test = case getLoopVar (loops!!0) of 
	--	Just a -> checkForVariable a
	--	Nothing -> ["Empty"]
	--putStr $ show $ test
	--putStr "\n"

parseTest s = do f <- readFile s
                 return $ parse $ preProcess f

arrayElemTest :: (Typeable p, Data p) => Fortran p -> [Variable]
arrayElemTest codeSeg = everything (++) (mkQ [] getArrayElement_test) codeSeg

getArrayElement_test :: Fortran () -> [Variable]
getArrayElement_test codeSeg = case codeSeg of
	Assg _ _ (Var _ _ lst) expr2 -> foldl (getArrayElementVariables_foldl) [] lst
	_ -> []

paralleliseLoop :: [Variable] -> Fortran () -> Fortran ()
paralleliseLoop loopVars loop = case paralleliseLoop_map loop newLoopVars of 
									Just a -> a
									Nothing -> loop
								where
									newLoopVars = case getLoopVar loop of
										Just a -> loopVars ++ checkForVariable a
										Nothing -> loopVars

paralleliseLoop_map :: Fortran () -> [Variable] -> Maybe (Fortran ())
paralleliseLoop_map loop loopVars	|	checkAssignments_map loopVars loop= Just (arbitraryChange_allChildren "PARALLEL" loop)
									|	otherwise		= Nothing

checkAssignments_map :: [Variable] -> Fortran () -> Bool
checkAssignments_map loopVars codeSeg = case codeSeg of
		Assg _ _ (Var _ _ lst) expr2 -> contains_list_rejectEmpty loopVars (foldl (getArrayElementVariables_foldl) [] lst)
		For _ _ var _ _ _ _ -> all (== True) (gmapQ (mkQ True (checkAssignments_map (loopVars ++ (checkForVariable var) ))) codeSeg)
		_ -> all (== True) (gmapQ (mkQ True (checkAssignments_map loopVars)) codeSeg)

parallelisableLoop_reduce ::(Typeable p, Data p) => Fortran p -> Bool
parallelisableLoop_reduce loop = False

paralleliseProgram :: (Typeable p, Data p) => ProgUnit p -> ProgUnit p 
paralleliseProgram = everywhere (mkT (forTransform))

forTransform :: Fortran () -> Fortran ()
forTransform inp = case inp of
		For _ _ _ _ _ _ _ -> paralleliseLoop [] inp
		_ -> inp

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
getVarNames expr = everything (++) (mkQ [] checkForVarName) expr

getVariables :: (Typeable p, Data p) =>  [Expr p] -> [Variable]
getVariables expr = everything (++) (mkQ [] checkForVariable) expr

getVarNames_tst :: (Typeable p, Data p) =>  Fortran p -> [VarName p]
getVarNames_tst expr = everything (++) (mkQ [] checkForVarName) expr


	-- everything (++) (mkQ empty checkForAssignment) loop

checkForAssignment :: (Typeable p, Data p) => Fortran p -> [Fortran p]
checkForAssignment codeSeg = case codeSeg of
		Assg _ _ _ _ -> [codeSeg]
		_ -> []

getSrcSpan :: Data a => a -> SrcSpan
getSrcSpan codeSeg = (SrcLoc {srcFilename = "comment", srcLine = 10, srcColumn = -1}, 
							SrcLoc {srcFilename = "comment", srcLine = 10, srcColumn = -1})

checkForVarName :: (Typeable p, Data p) =>  VarName p -> [VarName p]
checkForVarName expr = [expr]

checkForVariable :: VarName () -> [Variable]
checkForVariable (VarName () var) = [var]

--checkAssignments_map :: [Variable] -> Fortran () -> Bool
--checkAssignments_map loopVars codeSeg = case codeSeg of
--		Assg _ _ (Var _ _ lst) expr2 -> contains_list loopVars (foldl (getArrayElementVariables_foldl) [] lst)
--		For _ _ var _ _ _ _ -> all (== True) (gmapQ (mkQ True (checkAssignments_map (loopVars ++ (checkForVariable var)))) codeSeg)
--		_ -> all (== True) (gmapQ (mkQ True (checkAssignments_map loopVars)) codeSeg)

--checkArrayAssignments :: (Typeable p, Data p, Eq p) => [VarName p] -> Fortran p -> Bool
--checkArrayAssignments loopVars assignment = case assignment of
--		Assg _ _ (Var _ _ lst) expr2 -> contains_list loopVars (foldl (getArrayElementVariables_foldl) [] lst)

--		-- all (== True) (Prelude.map (contains_list loopVars) (getVarNames lst))

--		--case lst!!0 of
--		--						((VarName _ _), []) -> False
--		--						_ -> True
--		_	-> False

-- foldl :: (a -> b -> a) -> a -> [b] -> a
--getArrayElementVariables_foldl :: [Expr p] -> (VarName p, [Expr p]) -> [Expr p]
--getArrayElementVariables_foldl prevList (varname, xs) = 

--getArrayElementVariables_foldl :: (Typeable p, Data p) => [VarName p] -> (VarName p, [Expr p]) -> [VarName p]
--getArrayElementVariables_foldl prev (var, exps) = prev ++ (getVarNames exps)

getArrayElementVariables_foldl :: (Typeable p, Data p) => [Variable] -> (VarName p, [Expr p]) -> [Variable]
getArrayElementVariables_foldl prev (var, exps) = prev ++ (getVariables exps)

--getArrayElementVariables :: (VarName p, [Expr p]) -> [VarName p]
--getArrayElementVariables inp = []

getLoopVar :: Fortran p -> Maybe(VarName p)
getLoopVar (For _ _ var _ _ _ _) = Just var
getLoopVar _ = Nothing

contains_list :: [Variable] -> [Variable] -> Bool
contains_list container contained = all (== True) (Prelude.map (\x -> elem x container) contained)

contains_list_rejectEmpty :: [Variable] -> [Variable] -> Bool
contains_list_rejectEmpty container [] = False
contains_list_rejectEmpty container contained = all (== True) (Prelude.map (\x -> elem x container) contained)

arbitraryChange_allChildren :: (Data a, Typeable a) => String -> Fortran a -> Fortran a
arbitraryChange_allChildren comment = everywhere (mkT (modifySrcSpan_allChildren comment)) 

modifySrcSpan_allChildren :: String -> SrcSpan -> SrcSpan
modifySrcSpan_allChildren comment (a, b) = (SrcLoc {srcFilename = comment, srcLine = 10, srcColumn = -1}, b)