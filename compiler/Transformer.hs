module Main where

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List
import PreProcessor
import System.Environment

main :: IO ()
main = do
	--a <- parseFile "../testFiles/arrayLoop.f95"
	
	args <- getArgs
	let filename = args!!0
	--a <- parseFile "../testFiles/arrayLoop.f95"
	parsedProgram <- parseFile filename
	let parallelisedProg = paralleliseProgram (parsedProgram!!0)

	putStr $ compileErrorListing parallelisedProg
	putStr "\n\n\n"

	putStr $ show $ (parsedProgram!!0)
	putStr "\n\n\n"

	putStr $ show $ parallelisedProg
	putStr "\n"

	--putStr "\n"

parseFile s = do delete_foldl <- readFile s
                 return $ parse $ preProcess delete_foldl

paralleliseLoop :: [VarName [String]] -> Fortran [String] -> Fortran [String]
paralleliseLoop loopVars loop 	=	case mapAttempt_bool of
										True	-> mapAttempt_ast
										False 	-> case reduceAttempt_bool of
													True 	-> reduceAttempt_ast
													False	-> reduceAttempt_ast
								--case paralleliseLoop_map loop newLoopVars of 
								--	Just a -> a
								--	Nothing -> loop
								where
									newLoopVars = case getLoopVar loop of
										Just a -> loopVars ++ [a]
										Nothing -> loopVars

									mapAttempt = paralleliseLoop_map loop newLoopVars
									mapAttempt_bool = fst mapAttempt
									mapAttempt_ast = snd mapAttempt

									reduceAttempt = paralleliseLoop_reduce mapAttempt_ast newLoopVars
									reduceAttempt_bool = fst reduceAttempt
									reduceAttempt_ast = snd reduceAttempt

paralleliseLoop_map :: Fortran [String] -> [VarName [String]] -> (Bool, Fortran [String])
paralleliseLoop_map loop loopVars 	|	errors_map == "" 	=	(True,
																	OpenCLMap [] generatedSrcSpan 
													 				(listRemoveDuplications (listSubtract (getVarNames_query loop) (Prelude.map (\(a, _, _, _) -> a) (loopConditions_query_recursive loop))) ) 
																	--(flattenLoopConditions Nothing (VarName [] "g_id") 
																	(loopConditions_query_recursive loop) --)   
																	--(removeLoopConstructs_trans loop))
																	(removeLoopConstructs_recursive loop))
									|	otherwise	=			(False, addAnnotation loop (outputTab ++ "Cannot map due to:\n" ++ errors_map))
									where
										errors_map = getErrors_map loopVars loop

--paralleliseLoop_map loop loopVars	|	checkAssignments_map loopVars loop = Just (OpenCLMap [] generatedSrcSpan 
--																	 				(listRemoveDuplications (listSubtract (getVarNames_query loop) (Prelude.map (\(a, _, _, _) -> a) (loopConditions_query_recursive loop))) ) 
--																					(flattenLoopConditions Nothing (VarName [] "g_id") (loopConditions_query_recursive loop))   
--																					(removeLoopConstructs_trans loop)) -- Just (arbitraryChange_allChildren "PARALLEL" loop)
--									|	otherwise		= Just (addAnnotation loop ("  Cannot map due to:\n" ++ (getErrors_map loopVars loop)))
									
									-- |	otherwise		= Nothing

--checkAssignments_map :: [VarName [String]] -> Fortran [String] -> Bool
--checkAssignments_map loopVars codeSeg = case codeSeg of
--		Assg _ srcspan expr1 expr2 -> 		(assignments /= [])	
--										&& 	(exprListContainsVarNames assignments loopVars) 	
--										&& 	(constantCheck_query assignments) 
--										&& 	(exprListContainsVarNames accesses loopVars) 	-- && (constantCheck_query accesses)
--			where
--				assignments = arrayAccesses_query expr1
--				accesses = arrayAccesses_query expr2
--		For _ _ var _ _ _ _ -> all (== True) (gmapQ (mkQ True (checkAssignments_map (loopVars ++ [var]) )) codeSeg)
--		_ -> all (== True) (gmapQ (mkQ True (checkAssignments_map loopVars)) codeSeg)

getErrors_map :: [VarName [String]] -> Fortran [String] -> String
getErrors_map loopVars codeSeg = case codeSeg of
		Assg _ srcspan expr1 expr2 ->	(if assignments == [] then 
												outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": assignment to scalar variable\n" else "")
										
										-- ++ (if not (constantCheck_query assignments) then 
										--		outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": assignment to constant array element\n" else "") ++
										
										++ (if not (all (== True) (map (exprListContainsAllVarNames loopVars) assignments)) then 
												outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": assignment to array element with non-loop variable dimensions\n" else "")

										++ (if (not (all (== True) (map (exprListContainsAllVarNames loopVars) accesses))) && accesses /= [] then 
												outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": access to array element with non-loop variable dimensions\n" else "")

										-- ++ (if not (exprListContainsVarNames assignments loopVars) then 
										--		outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": assignment to array element at non-loop variable position\n" else "") ++
										
										-- ++ (if not (exprListContainsVarNames accesses loopVars) then 
										--		outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": access to an array element not related to the loop current loop variable scope\n" else "")
		--(assignments /= [])	&& (exprListContainsVarNames assignments loopVars) 	&& (constantCheck_query assignments) 
		--											&& (exprListContainsVarNames accesses loopVars) 	-- && (constantCheck_query accesses)
			where
				assignments = arrayAccesses_query expr1
				accesses = arrayAccesses_query expr2
		For _ _ var _ _ _ _ -> foldl (++) "" (gmapQ (mkQ "" (getErrors_map (loopVars ++ [var]) )) codeSeg)
		OpenCLMap _ _ _ _ _ -> foldl (++) "" (gmapQ (mkQ "" (getErrors_map loopVars)) codeSeg)
		_ -> foldl (++) "" (gmapQ (mkQ "" (getErrors_map loopVars)) codeSeg)

addAnnotation :: Fortran [String] -> String -> Fortran [String]
addAnnotation original appendage = case original of
		For anno srcspan var expr1 expr2 expr3 fortran -> For (anno ++ [appendage]) srcspan var expr1 expr2 expr3 fortran
		_ -> original		

errorLocationFormatting :: SrcSpan -> String
errorLocationFormatting ((SrcLoc filename line column), srcEnd) = "line " ++ show line -- ++ ", column " ++ show column

paralleliseLoop_reduce ::Fortran [String] -> [VarName [String]] -> (Bool, Fortran [String])
paralleliseLoop_reduce loop loopVars = (False, addAnnotation loop (outputTab ++ "Cannot reduce due to:\n" ++ outputTab ++ outputTab ++ "Reduction is not implemented\n"))

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
		OpenCLMap _ _ _ loopVars _ -> loopVars
		_ -> []

checkLoop inp = case inp of
		For _ _ _ _ _ _ _ -> [inp]
		_ -> []

flattenLoopConditions :: Maybe (VarName p) -> (VarName p) -> [(VarName p, Expr p, Expr p, Expr p)] -> Fortran p
flattenLoopConditions prev globalId ((var, start, end, step):[]) = Assg 
																		(tag globalId) 
																		generatedSrcSpan 
																		(Var (tag globalId) generatedSrcSpan [(var, [])])
																		(primitiveMod globalId end)
flattenLoopConditions prev globalId ((var, start, end, step):xs) = 	FSeq 
																	(tag globalId) 
																	generatedSrcSpan (
																		Assg 
																		(tag globalId) 
																		generatedSrcSpan (
																			Var (tag globalId) generatedSrcSpan [(var, [])])
																		(flattenCondition_div globalId prev (multiplyLoopConditions xs) -- DIVISOR
																			)
																		)
																	 (flattenLoopConditions (Just var) globalId xs) -- FSeq p SrcSpan (Fortran p) (Fortran p) 

flattenCondition_div :: VarName p -> Maybe (VarName p) -> Expr p -> Expr p
flattenCondition_div globalId (Just prev) divisor = Bin 
														(tag globalId) 
														generatedSrcSpan 
														(Div (tag globalId)) (
															Bin 
																(tag globalId) 
																generatedSrcSpan 
																(Minus (tag globalId)) 
																(Var 
																	(tag globalId) 
																	generatedSrcSpan 
																	[(globalId, [])]) 
																(Var 
																	(tag globalId) 
																	generatedSrcSpan 
																	[(prev, [])]))
														divisor
flattenCondition_div globalId Nothing divisor = 	Bin 
														(tag globalId) 
														generatedSrcSpan 
														(Div (tag globalId)) 
														(Var 
															(tag globalId) 
															generatedSrcSpan 
															[(globalId, [])]) 
														divisor

flattenCondition_mod :: VarName p -> Maybe (VarName p) -> Expr p -> Expr p
flattenCondition_mod globalId (Just prev) divisor = Bin 
														(tag globalId) 
														generatedSrcSpan 
														(Div (tag globalId)) 
														(Var 
															(tag globalId) 
															generatedSrcSpan 
															[(globalId, [])]) 
														divisor 

primitiveMod :: VarName p -> Expr p -> Expr p 
primitiveMod quotient divisor = Bin 
									(tag quotient) 
									generatedSrcSpan 
									(Minus (tag quotient)) 
									(Var 
										(tag quotient) 
										generatedSrcSpan 
										[(quotient, [])]) 
									(Bin 
										(tag quotient) 
										generatedSrcSpan 
										(Mul (tag quotient)) 
										(Bin 
											(tag quotient) 
											generatedSrcSpan 
											(Div (tag quotient)) 
											(Var 
												(tag quotient) 
												generatedSrcSpan 
												[(quotient, [])]) 
											divisor) 
										divisor)

multiplyLoopConditions :: [(VarName p, Expr p, Expr p, Expr p)] -> Expr p
multiplyLoopConditions ((var, start, end, step):[]) = end
multiplyLoopConditions ((var, start, end, step):xs) = Bin (tag var) generatedSrcSpan (Mul (tag var)) end (multiplyLoopConditions xs)
													-- Bin p SrcSpan  (BinOp p) (Expr p) (Expr p)			

--removeLoopConstructs_trans :: (Typeable p, Data p) => Fortran p -> Fortran p
--removeLoopConstructs_trans = everywhere (mkT (removeLoopConstructs))

removeLoopConstructs_recursive :: Fortran [String] -> Fortran [String]
removeLoopConstructs_recursive (FSeq _ _ (For _ _ _ _ _ _ (FSeq anno srcspan fortran11 fortran12)) fortran02) = FSeq anno srcspan fortran11 
																															(appendFortran_recursive 	(removeLoopConstructs_recursive fortran02) 
																																						(removeLoopConstructs_recursive fortran12))
removeLoopConstructs_recursive (FSeq _ _ (OpenCLMap _ _ _ _ (FSeq anno srcspan fortran11 fortran12)) fortran02) = FSeq anno srcspan fortran11 
																															(appendFortran_recursive 	(removeLoopConstructs_recursive fortran02) 
																																						(removeLoopConstructs_recursive fortran12))
removeLoopConstructs_recursive (For _ _ _ _ _ _ fortran1) = removeLoopConstructs_recursive fortran1
removeLoopConstructs_recursive (OpenCLMap _ _ _ _ fortran1) = removeLoopConstructs_recursive fortran1
removeLoopConstructs_recursive (NullStmt anno srcspan) = NullStmt anno srcspan
removeLoopConstructs_recursive codeSeg = gmapT (mkT removeLoopConstructs_recursive) codeSeg


--removeLoopConstructs :: Fortran [String] -> Fortran [String]
----removeLoopConstructs (FSeq _ _ (For _ _ _ _ _ _ (FSeq _ _ fortran11 fortran12)) fortran02) = FSeq [] generatedSrcSpan fortran11 (appendFortran_trans fortran02 fortran12)
--removeLoopConstructs (FSeq _ _ (For _ _ _ _ _ _ (FSeq _ _ fortran11 fortran12)) fortran02) = FSeq [] generatedSrcSpan fortran11 (appendFortran_recursive fortran02 fortran12)
--removeLoopConstructs (FSeq _ _ (For _ _ _ _ _ _ fortran1) fortran2 ) = FSeq [] generatedSrcSpan fortran1 fortran2
--removeLoopConstructs (FSeq _ _ fortran1 (For _ _ _ _ _ _ fortran2)) = FSeq [] generatedSrcSpan fortran1 fortran2

----removeLoopConstructs (FSeq _ _ (OpenCLMap _ _ _ _ (FSeq _ _ fortran11 fortran12)) fortran02) = FSeq [] generatedSrcSpan fortran11 (appendFortran_trans fortran02 fortran12)
--removeLoopConstructs (FSeq _ _ (OpenCLMap _ _ _ _ (FSeq _ _ fortran11 fortran12)) fortran02) = FSeq [] generatedSrcSpan fortran11 (appendFortran_recursive fortran02 fortran12)
--removeLoopConstructs (FSeq _ _ (OpenCLMap _ _ _ _ fortran1) fortran2 ) = FSeq [] generatedSrcSpan fortran1 fortran2
--removeLoopConstructs (FSeq _ _ fortran1 (OpenCLMap _ _ _ _ fortran2)) = FSeq [] generatedSrcSpan fortran1 fortran2
--removeLoopConstructs (FSeq _ _ fortran1 (NullStmt _ _)) = fortran1

--removeLoopConstructs (For _ _ _ _ _ _ fortran) = fortran
--removeLoopConstructs (OpenCLMap _ _ _ _ fortran) = fortran
--removeLoopConstructs codeSeg = codeSeg

appendFortran_trans :: Fortran [String] -> Fortran [String] -> Fortran [String]
appendFortran_trans newFortran codeSeg = everywhere (mkT (appendFortran newFortran)) codeSeg

appendFortran :: Fortran [String] -> Fortran [String] -> Fortran [String]
appendFortran newFortran codeSeg = case codeSeg of
	(FSeq _ _ _ (FSeq _ _ _ _)) -> codeSeg 
 	(FSeq _ _ fortran1 fortran2) -> FSeq [] generatedSrcSpan fortran1 (FSeq [] generatedSrcSpan fortran2 newFortran)
 	_ -> codeSeg
 	-- _ -> FSeq [] generatedSrcSpan codeSeg newFortran

appendFortran_recursive :: Fortran [String] -> Fortran [String] -> Fortran [String]
appendFortran_recursive newFortran (FSeq _ _ _ (FSeq _ _ _ fortran1)) = appendFortran_recursive newFortran fortran1 
appendFortran_recursive newFortran (FSeq _ _ fortran1 fortran2) = FSeq [] generatedSrcSpan fortran1 (FSeq [] generatedSrcSpan fortran2 newFortran)
appendFortran_recursive newFortran codeSeg = FSeq [] generatedSrcSpan codeSeg newFortran

compileErrorListing :: ProgUnit [String] -> String
compileErrorListing codeSeg = everything (++) (mkQ [] getErrors) codeSeg

getErrors :: Fortran [String] -> String
getErrors codeSeg = case tag codeSeg of
	[] -> ""
	_ -> "Loop at " ++ (errorLocationFormatting (srcSpan codeSeg)) ++ " cannot be parallelised.\n" ++ (foldl (++) "" (tag codeSeg)) ++ "\n"

--getErrors (For tag srcspan _ _ _ _ _) = "For loop at " ++ (errorLocationFormatting srcspan) ++ " cannot be parallelised." ++ (foldl (++) "" tag) ++ "\n"
--getErrors codeSeg = "Loop at " ++ (errorLocationFormatting (srcSpan codeSeg)) ++ " cannot be parallelised." ++ (foldl (++) "" (tag codeSeg)) ++ "\n"

getVarNames_query :: (Typeable p, Data p) =>  Fortran p -> [VarName p]
getVarNames_query fortran = everything (++) (mkQ [] getVarNames) fortran

getVarNames :: (Typeable p, Data p) =>  VarName p -> [VarName p]
getVarNames expr = [expr]

listSubtract :: Eq a => [a] -> [a] -> [a]
listSubtract a b = filter (\x -> notElem x b) a

listRemoveDuplications :: Eq a => [a] -> [a]
listRemoveDuplications a = foldl (\accum item -> if notElem item accum then accum ++ [item] else accum) [] a

arrayAccesses_query :: (Typeable p, Data p) =>  Expr p -> [[Expr p]]
arrayAccesses_query codeSeg = case codeSeg of
	Var _ _ lst -> [foldl concatExprList_foldl [] lst]
	Bin _ _ _ expr1 expr2 -> arrayAccesses_query expr1 ++ arrayAccesses_query expr2
	_ -> []

-- (gmapQ (mkQ "" (getErrors_map loopVars)) codeSeg)

--arrayAccesses_query :: (Typeable p, Data p) =>  Expr p -> [Expr p]
--arrayAccesses_query = everything (++) (mkQ [] getArrayAccesses)

--getArrayAccesses :: (Typeable p, Data p) => Expr p -> [Expr p]
--getArrayAccesses codeSeg = case codeSeg of
--	Var _ _ lst -> foldl concatExprList_foldl [] lst
--	_ -> []

concatExprList_foldl :: (Typeable p, Data p) => [Expr p] -> (VarName p, [Expr p]) -> [Expr p]
concatExprList_foldl prev (var, exprs) = prev ++ exprs 

--exprListContainsVarNames accesses loopVars

exprListContainsVarNames :: (Typeable p, Data p, Eq p) =>  [Expr p] -> [VarName p] -> Bool
exprListContainsVarNames contains container = all (== True) (everything (++) (mkQ [] (varNameCheck container)) contains)

exprListContainsAllVarNames :: (Typeable p, Data p, Eq p) => [VarName p] -> [Expr p] -> Bool
exprListContainsAllVarNames contains container = (foldl delete_foldl (listRemoveDuplications contains) (everything (++) (mkQ [] getVarNames) container)) == []

--exprListContainsAllVarNames :: (Typeable p, Data p, Eq p) =>  [Expr p] -> [VarName p] -> Bool
--exprListContainsAllVarNames container contains = (foldl delete_foldl (listRemoveDuplications contains) (everything (++) (mkQ [] getVarNames) container)) == []

delete_foldl :: (Typeable p, Data p, Eq p) => [VarName p] -> VarName p -> [VarName p]
delete_foldl accum item = delete item accum

varNameCheck :: (Typeable p, Data p, Eq p) => [VarName p] -> VarName p -> [Bool]
varNameCheck container contains = [elem contains container]

	--all (== True) (everything (++) (mkQ [] (allVarNameCheck container)) contains)

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

outputTab :: String
outputTab = "  "


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

generatedSrcSpan :: SrcSpan
generatedSrcSpan = (SrcLoc {srcFilename = "GENERATED", srcLine = -1, srcColumn = -1}, SrcLoc {srcFilename = "GENERATED", srcLine = -1, srcColumn = -1})

test_exprListContainsAllVarNames :: (Typeable p, Data p, Eq p) =>  [Expr p] -> [VarName p] -> [VarName p]
test_exprListContainsAllVarNames container contains = (foldl delete_foldl (listRemoveDuplications contains) (everything (++) (mkQ [] getVarNames) container))