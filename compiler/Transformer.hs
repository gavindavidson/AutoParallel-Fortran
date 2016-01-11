module Main where

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import PreProcessor

main :: IO ()
main = do
	--a <- parseTest "../testFiles/arrayLoop.f95"
	a <- parseTest "../testFiles/arrayLoop.f95"
	let parallelisedProg = paralleliseProgram (a!!0)

	putStr $ show $ (a!!0)
	putStr "\n\n\n"

	putStr $ show $ parallelisedProg
	putStr "\n"

parseTest s = do f <- readFile s
                 return $ parse $ preProcess f

paralleliseLoop :: [VarName [String]] -> Fortran [String] -> Fortran [String]
paralleliseLoop loopVars loop = case paralleliseLoop_map loop newLoopVars of 
									Just a -> a
									Nothing -> loop
								where
									newLoopVars = case getLoopVar loop of
										Just a -> loopVars ++ [a]
										Nothing -> loopVars

paralleliseLoop_map :: Fortran [String] -> [VarName [String]] -> Maybe (Fortran [String])
paralleliseLoop_map loop loopVars	|	checkAssignments_map loopVars loop = Just (OpenCLMap [] dummySrcSpan 
																	 				(listRemoveDuplications (listSubtract (getVarNames_query loop) (Prelude.map (\(a, _, _, _) -> a) (loopConditions_query_recursive loop))) ) 
																					(flattenLoopConditions Nothing (VarName [] "g_id") (loopConditions_query_recursive loop))   
																					(removeLoopConstructs_trans loop)) -- Just (arbitraryChange_allChildren "PARALLEL" loop)
									|	otherwise		= Nothing

checkAssignments_map :: [VarName [String]] -> Fortran [String] -> Bool
checkAssignments_map loopVars codeSeg = case codeSeg of
		Assg _ _ expr1 expr2 -> (assignments /= [])	&& (exprListContainsVarNames assignments loopVars) 	&& (constantCheck_query assignments) 
													&& (exprListContainsVarNames accesses loopVars) 	-- && (constantCheck_query accesses)
			where
				assignments = arrayAccesses_query expr1
				accesses = arrayAccesses_query expr2
		For _ _ var _ _ _ _ -> all (== True) (gmapQ (mkQ True (checkAssignments_map (loopVars ++ [var]) )) codeSeg)
		_ -> all (== True) (gmapQ (mkQ True (checkAssignments_map loopVars)) codeSeg)

parallelisableLoop_reduce ::(Typeable p, Data p) => Fortran p -> Bool
parallelisableLoop_reduce loop = False

paralleliseProgram :: (Typeable p, Data p) => ProgUnit p -> ProgUnit p 
paralleliseProgram = everywhere (mkT (transformForLoop))

transformForLoop :: Fortran [String] -> Fortran [String]
transformForLoop inp = case inp of
		For _ _ _ _ _ _ _ -> paralleliseLoop [] inp
		_ -> inp

loopConditions_query_recursive :: (Typeable p, Data p) =>  Fortran p -> [(VarName p, Expr p, Expr p, Expr p)]
loopConditions_query_recursive = everything (++) (mkQ [] getLoopConditions)

getLoopConditions :: (Typeable p, Data p) => Fortran p -> [(VarName p, Expr p, Expr p, Expr p)]
getLoopConditions codeSeg = case codeSeg of
		For _ _ var start end step _ -> [(var, start, end, step)]
		_ -> []

checkLoop inp = case inp of
		For _ _ _ _ _ _ _ -> [inp]
		_ -> []

flattenLoopConditions :: Maybe (VarName p) -> (VarName p) -> [(VarName p, Expr p, Expr p, Expr p)] -> Fortran p
flattenLoopConditions prev globalId ((var, start, end, step):[]) = Assg 
																		(tag globalId) 
																		dummySrcSpan 
																		(Var (tag globalId) dummySrcSpan [(var, [])])
																		(primitiveMod globalId end)
flattenLoopConditions prev globalId ((var, start, end, step):xs) = 	FSeq 
																	(tag globalId) 
																	dummySrcSpan (
																		Assg 
																		(tag globalId) 
																		dummySrcSpan (
																			Var (tag globalId) dummySrcSpan [(var, [])])
																		(flattenCondition_div globalId prev (multiplyLoopConditions xs) -- DIVISOR
																			)
																		)
																	 (flattenLoopConditions (Just var) globalId xs) -- FSeq p SrcSpan (Fortran p) (Fortran p) 

flattenCondition_div :: VarName p -> Maybe (VarName p) -> Expr p -> Expr p
flattenCondition_div globalId (Just prev) divisor = Bin 
														(tag globalId) 
														dummySrcSpan 
														(Div (tag globalId)) (
															Bin 
																(tag globalId) 
																dummySrcSpan 
																(Minus (tag globalId)) 
																(Var 
																	(tag globalId) 
																	dummySrcSpan 
																	[(globalId, [])]) 
																(Var 
																	(tag globalId) 
																	dummySrcSpan 
																	[(prev, [])]))
														divisor
flattenCondition_div globalId Nothing divisor = 	Bin 
														(tag globalId) 
														dummySrcSpan 
														(Div (tag globalId)) 
														(Var 
															(tag globalId) 
															dummySrcSpan 
															[(globalId, [])]) 
														divisor

flattenCondition_mod :: VarName p -> Maybe (VarName p) -> Expr p -> Expr p
flattenCondition_mod globalId (Just prev) divisor = Bin 
														(tag globalId) 
														dummySrcSpan 
														(Div (tag globalId)) 
														(Var 
															(tag globalId) 
															dummySrcSpan 
															[(globalId, [])]) 
														divisor 

primitiveMod :: VarName p -> Expr p -> Expr p 
primitiveMod quotient divisor = Bin 
									(tag quotient) 
									dummySrcSpan 
									(Minus (tag quotient)) 
									(Var 
										(tag quotient) 
										dummySrcSpan 
										[(quotient, [])]) 
									(Bin 
										(tag quotient) 
										dummySrcSpan 
										(Mul (tag quotient)) 
										(Bin 
											(tag quotient) 
											dummySrcSpan 
											(Div (tag quotient)) 
											(Var 
												(tag quotient) 
												dummySrcSpan 
												[(quotient, [])]) 
											divisor) 
										divisor)

multiplyLoopConditions :: [(VarName p, Expr p, Expr p, Expr p)] -> Expr p
multiplyLoopConditions ((var, start, end, step):[]) = end
multiplyLoopConditions ((var, start, end, step):xs) = Bin (tag var) dummySrcSpan (Mul (tag var)) end (multiplyLoopConditions xs)
													-- Bin p SrcSpan  (BinOp p) (Expr p) (Expr p)			

removeLoopConstructs_trans :: (Typeable p, Data p) => Fortran p -> Fortran p
removeLoopConstructs_trans = everywhere (mkT (removeLoopConstructs))

removeLoopConstructs :: Fortran [String] -> Fortran [String]
removeLoopConstructs (FSeq [] _ (For _ _ _ _ _ _ (FSeq [] _ fortran11 fortran12)) fortran02) = FSeq [] dummySrcSpan fortran11 (appendFortran_trans fortran02 fortran12)
removeLoopConstructs (FSeq [] _ (For _ _ _ _ _ _ fortran1) fortran2 ) = FSeq [] dummySrcSpan fortran1 fortran2
removeLoopConstructs (FSeq [] _ fortran1 (For _ _ _ _ _ _ fortran2)) = FSeq [] dummySrcSpan fortran1 fortran2
removeLoopConstructs (FSeq [] _ fortran1 (NullStmt [] _)) = fortran1
removeLoopConstructs (For _ _ _ _ _ _ fortran) = fortran
removeLoopConstructs codeSeg = codeSeg

appendFortran_trans :: Fortran [String] -> Fortran [String] -> Fortran [String]
appendFortran_trans newFortran codeSeg = everywhere (mkT (appendFortran newFortran)) codeSeg

appendFortran :: Fortran [String] -> Fortran [String] -> Fortran [String]
appendFortran newFortran codeSeg = case codeSeg of
	(FSeq [] _ _ (FSeq [] _ _ _)) -> codeSeg 
 	(FSeq [] _ fortran1 fortran2) -> FSeq [] dummySrcSpan fortran1 (FSeq [] dummySrcSpan fortran2 newFortran)
 	_ -> codeSeg

getVarNames_query :: (Typeable p, Data p) =>  Fortran p -> [VarName p]
getVarNames_query fortran = everything (++) (mkQ [] getVarNames) fortran

getVarNames :: (Typeable p, Data p) =>  VarName p -> [VarName p]
getVarNames expr = [expr]

listSubtract :: Eq a => [a] -> [a] -> [a]
listSubtract a b = filter (\x -> notElem x b) a

listRemoveDuplications :: Eq a => [a] -> [a]
listRemoveDuplications a = foldl (\accum item -> if notElem item accum then accum ++ [item] else accum) [] a

arrayAccesses_query :: (Typeable p, Data p) =>  Expr p -> [Expr p]
arrayAccesses_query = everything (++) (mkQ [] getArrayAccesses)

getArrayAccesses :: (Typeable p, Data p) => Expr p -> [Expr p]
getArrayAccesses codeSeg = case codeSeg of
	Var _ _ lst -> foldl concatExprList_foldl [] lst
	_ -> []

concatExprList_foldl :: (Typeable p, Data p) => [Expr p] -> (VarName p, [Expr p]) -> [Expr p]
concatExprList_foldl prev (var, exprs) = prev ++ exprs 

exprListContainsVarNames :: (Typeable p, Data p, Eq p) =>  [Expr p] -> [VarName p] -> Bool
exprListContainsVarNames contains container = all (== True) (everything (++) (mkQ [] (varNameCheck container)) contains)

varNameCheck :: (Typeable p, Data p, Eq p) => [VarName p] -> VarName p -> [Bool]
varNameCheck container contains = [elem contains container]

constantCheck_query :: (Typeable p, Data p) => [Expr p] -> Bool
constantCheck_query exprList = all (== False) (everything (++) (mkQ [] (constantCheck)) exprList)

constantCheck :: Expr [String] -> [Bool]
constantCheck (Con _ _ _) = [True]
constantCheck _ = [False]

accessVarCheck_query :: (Typeable p, Data p, Eq p) => [VarName p] -> [[Expr p]] -> Bool
accessVarCheck_query loopVars exprList = all (== True) (everything (++) (mkQ [] (accessVarCheck loopVars)) exprList)

accessVarCheck :: (Typeable p, Data p, Eq p) => [VarName p] -> VarName p -> [Bool]
accessVarCheck loopVars varname = [elem varname loopVars]

getLoopVar :: Fortran p -> Maybe(VarName p)
getLoopVar (For _ _ var _ _ _ _) = Just var
getLoopVar _ = Nothing



-- FUNCTIONS FOR DEBUGGING AND DEVELOPMENT

identifyLoops :: (Typeable p, Data p) => ProgUnit p -> [Fortran [String]]
identifyLoops program =
	everything
		(++)
		(mkQ [] checkLoop)
		program

contains_list :: [Variable] -> [Variable] -> Bool
contains_list container contained = all (== True) (Prelude.map (\x -> elem x container) contained)

contains_list_rejectEmpty :: [Variable] -> [Variable] -> Bool
contains_list_rejectEmpty container [] = False
contains_list_rejectEmpty container contained = all (== True) (Prelude.map (\x -> elem x container) contained)

arbitraryChange_allChildren :: (Data a, Typeable a) => String -> Fortran a -> Fortran a
arbitraryChange_allChildren comment = everywhere (mkT (modifySrcSpan_allChildren comment)) 

modifySrcSpan_allChildren :: String -> SrcSpan -> SrcSpan
modifySrcSpan_allChildren comment (a, b) = (SrcLoc {srcFilename = comment, srcLine = 10, srcColumn = -1}, b)

dummySrcSpan :: SrcSpan
dummySrcSpan = (SrcLoc {srcFilename = "dummy", srcLine = -1, srcColumn = -1}, SrcLoc {srcFilename = "dummy", srcLine = -1, srcColumn = -1})