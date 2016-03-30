module LoopAnalysis where

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran
import Data.Char
import Data.List
import qualified Data.Map as DMap 

import VarAccessAnalysis
import VarDependencyAnalysis
import LanguageFortranTools

--	Type used to standardise loop analysis functions
--						errors 		reduction variables read variables		written variables		
type AnalysisInfo = 	(Anno, 	[Expr Anno], 		[Expr Anno], 		[Expr Anno])

analysisInfoBaseCase :: AnalysisInfo
analysisInfoBaseCase = (nullAnno,[],[],[])

combineAnalysisInfo :: AnalysisInfo -> AnalysisInfo -> AnalysisInfo
combineAnalysisInfo accum item = (combineMaps accumErrors itemErrors, accumReductionVars ++ itemReductionVars, accumReads ++ itemReads, accumWrites ++ itemWrites)
								where
									(accumErrors, accumReductionVars, accumReads, accumWrites) = accum
									(itemErrors, itemReductionVars, itemReads, itemWrites) = item

--	Function takes a list of loop variables and a possible parallel loop's AST and returns a string that details the reasons why the loop
--	cannot be mapped. If the returned string is empty, the loop represents a possible parallel map
analyseLoop_map :: [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarAccessAnalysis -> VarDependencyAnalysis -> Fortran Anno -> AnalysisInfo
analyseLoop_map loopVars loopWrites nonTempVars accessAnalysis dependencies codeSeg = case codeSeg of
		If _ _ expr _ elifList maybeElse -> foldl combineAnalysisInfo analysisInfoBaseCase (generalAnalysis ++ elifAnalysis_fortran ++ elifAnalysis_readExprs ++ [elseAnalysis])
						where
							generalAnalysis = gmapQ (mkQ analysisInfoBaseCase (analyseLoop_map (loopVars) loopWrites nonTempVars accessAnalysis dependencies)) codeSeg
							elifAnalysis_fortran = map (\(_, elif_fortran) -> analyseLoop_map (loopVars) loopWrites nonTempVars accessAnalysis dependencies elif_fortran) elifList
							elifAnalysis_readExprs = map (\(elif_expr, _) -> (nullAnno,[],extractOperands elif_expr,[])) elifList
							elseAnalysis = case maybeElse of
												Just else_fortran ->  analyseLoop_map (loopVars) loopWrites nonTempVars accessAnalysis dependencies else_fortran
												Nothing -> analysisInfoBaseCase
		Assg _ srcspan expr1 expr2 -> foldl (combineAnalysisInfo) analysisInfoBaseCase [expr1Analysis, expr2Analysis, (loopCarriedDependencyErrorMap,[],expr2Operands,[expr1])] -- combineAnalysisInfo (combineAnalysisInfo expr1Analysis expr2Analysis) (nullAnno,[],expr2Operands,[expr1])
						where
							expr1Analysis = (analyseAccess "Cannot map: " loopVars loopWrites nonTempVars accessAnalysis expr1)
							expr2Analysis = (analyseAccess "Cannot map: " loopVars loopWrites nonTempVars accessAnalysis expr2)
							expr2Operands = extractOperands expr2

							loopCarriedDependencyList = loopCarriedDependencyCheck loopVars dependencies expr1
							loopCarriedDependencyBool = loopCarriedDependencyList /= []
							loopCarriedDependencyErrorMap = if loopCarriedDependencyBool then
											DMap.insert (outputTab ++ "Cannot reduce: Loop carried dependency on " ++ outputExprFormatting expr1 ++ ":\n")
												(map (\item -> errorLocationFormatting (srcSpan item) ++ outputTab ++ outputExprFormatting item) loopCarriedDependencyList)
												DMap.empty
											else DMap.empty

		For _ _ var _ _ _ _ -> foldl combineAnalysisInfo analysisInfoBaseCase childrenAnalysis
						where
							childrenAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_map (loopVars ++ [var]) loopWrites nonTempVars accessAnalysis dependencies)) codeSeg)
		Call _ srcspan expr arglist -> (errorMap_call, [], [], argExprs)
						where
							errorMap_call = DMap.insert (outputTab ++ "Cannot reduce: Call to external function:\n")
															[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr]
															DMap.empty
							argExprs = everything (++) (mkQ [] extractExpr_list) arglist
		_ -> foldl combineAnalysisInfo analysisInfoBaseCase (childrenAnalysis ++ nodeAccessAnalysis)
						where
							nodeAccessAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseAccess "Cannot map: " loopVars loopWrites nonTempVars accessAnalysis)) codeSeg)
							childrenAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_map loopVars loopWrites nonTempVars accessAnalysis dependencies)) codeSeg)

--	Function takes a list of loop variables and a possible parallel loop's AST and returns a string that details the reasons why the loop
--	doesn't represent a reduction. If the returned string is empty, the loop represents a possible parallel reduction
analyseLoop_reduce :: [Expr Anno] -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarDependencyAnalysis -> VarAccessAnalysis -> Fortran Anno -> AnalysisInfo
analyseLoop_reduce condExprs loopVars loopWrites nonTempVars dependencies accessAnalysis codeSeg = case codeSeg of
		If _ _ expr _ elifList maybeElse -> foldl combineAnalysisInfo analysisInfoBaseCase (generalAnalysis ++ elifAnalysis_fortran ++ elifAnalysis_readExprs ++ [elseAnalysis])
							where
								generalAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce (condExprs ++ [expr]) loopVars loopWrites nonTempVars dependencies accessAnalysis)) codeSeg)
								elifAnalysis_fortran = map (\(elif_expr, elif_fortran) -> analyseLoop_reduce (condExprs ++ [elif_expr]) loopVars loopWrites nonTempVars dependencies accessAnalysis elif_fortran) elifList
								elifAnalysis_readExprs = map (\(elif_expr, _) -> (nullAnno,[],extractOperands elif_expr,[])) elifList
								elseAnalysis = case maybeElse of
												Just else_fortran -> analyseLoop_reduce (condExprs) loopVars loopWrites nonTempVars dependencies accessAnalysis else_fortran
												Nothing -> analysisInfoBaseCase
		For _ _ var _ _ _ _ -> foldl combineAnalysisInfo analysisInfoBaseCase childrenAnalysis
							where
								childrenAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce condExprs (loopVars ++ [var]) loopWrites nonTempVars dependencies accessAnalysis)) codeSeg)
		Assg _ srcspan expr1 expr2 -> 	combineAnalysisInfo
											(errorMap4
											,
											if potentialReductionVar then [expr1] else [],
											extractOperands expr2,
											[expr1])
											(if not potentialReductionVar then
												expr1Analysis
												else analysisInfoBaseCase)
			where
				writtenExprs = extractOperands expr1
				readOperands = extractOperands expr2
				readExprs = foldl (\accum item -> if isFunctionCall accessAnalysis item then accum ++ (extractContainedVars item) else accum ++ [item]) [] readOperands

				dependsOnSelfOnce = (length (filter (\item -> item == writtenVarname) readVarnames)) == 1

				writtenVarname = head $ foldl (\accum item -> accum ++ extractVarNames item) [] writtenExprs
				readVarnames 	= foldl (\accum item -> accum ++ extractVarNames item) [] readExprs

				isNonTempAssignment = usesVarName_list nonTempVars expr1
				
				referencedCondition = (foldl (||) False $ map (\x -> hasOperand x expr1) condExprs)
				referencedSelf = (hasOperand expr2 expr1)
				associative = isAssociativeExpr expr1 expr2

				dependsOnSelf = referencedSelf || referencedCondition || dependsOnSelfOnce 
									-- || (foldl (||) False $ map (\x -> isIndirectlyDependentOn dependencies x x) writtenVarnames)
									|| (foldl (||) False $ map (\x -> isIndirectlyDependentOn dependencies (head $ extractVarNames x) x) writtenExprs)
				--usesFullLoopVarError = analyseAccess_reduce loopVars loopWrites nonTempVars accessAnalysis expr1
				
				expr1Analysis = (analyseAccess "Cannot reduce: " loopVars loopWrites nonTempVars accessAnalysis expr1)
				expr2Analysis = (analyseAccess "Cannot reduce: " loopVars loopWrites nonTempVars accessAnalysis expr2)
				--usesFullLoopVarError = analyseAccess_reduce loopVars loopWrites nonTempVars accessAnalysis expr1
				doesNotUseFullLoopVar = (\(errorMap, _, _, _) -> errorMap == nullAnno) expr1Analysis

				potentialReductionVar = isNonTempAssignment && (dependsOnSelf) -- && doesNotUseFullLoopVar
				--potentialReductionVar = isNonTempAssignment && (referencedSelf || referencedCondition) && ((\(str, _, _, _) -> str == "") usesFullLoopVarError)

				--errorMap1 = if (not potentialReductionVar) && isNonTempAssignment then 
				--errorMap1 = if (not usesFullLoopVarBool) && isNonTempAssignment then 
				--							DMap.insert (outputTab ++ "Cannot reduce: The following variables do not use all loop iterators:\n" )
				--								[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr1]
				--								DMap.empty
				--							else DMap.empty

				loopCarriedDependencyList = loopCarriedDependencyCheck loopVars dependencies expr1
				loopCarriedDependencyBool = loopCarriedDependencyList /= []

				errorMap1 = DMap.empty
				errorMap2 = if potentialReductionVar && (not dependsOnSelfOnce) then
											DMap.insert (outputTab ++ "Cannot reduce: Possible reduction variables must only appear once on the right hand side of an assignment:\n")
												[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr1]
												errorMap1
											else errorMap1
				errorMap3 = if potentialReductionVar && (not associative) && dependsOnSelfOnce then
											DMap.insert (outputTab ++ "Cannot reduce: Not associative function:\n")
												[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr2]
												errorMap2
											else errorMap2
				errorMap4 = if loopCarriedDependencyBool then
											DMap.insert (outputTab ++ "Cannot reduce: Loop carried dependency on " ++ outputExprFormatting expr1 ++ ":\n")
												(map (\item -> errorLocationFormatting (srcSpan item) ++ outputTab ++ outputExprFormatting item) loopCarriedDependencyList)
												-- [errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr2]
												errorMap3
											else errorMap3
				--errorMap4 = if not dependsOnSelf then
				--							DMap.insert (outputTab ++ "Cannot reduce: The following variables are not assigned values dependent on themselves:\n")
				--								[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr1]
				--								errorMap3
				--							else errorMap3
				--errorMap_debug = if referencedSelf then
				--							DMap.insert (outputTab ++ "References self:\n")
				--								[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr1]
				--								errorMap3
				--							else errorMap3
				--errorMap_debug' = if isNonTempAssignment then
				--							DMap.insert (outputTab ++ "Non temp assignments:\n")
				--								[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr1]
				--								errorMap_debug
				--							else errorMap_debug
		Call _ srcspan expr arglist -> (errorMap_call, [], [], argExprs)
			where
				errorMap_call = DMap.insert (outputTab ++ "Cannot reduce: Call to external function:\n")
												[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr]
												DMap.empty
				argExprs = everything (++) (mkQ [] extractExpr_list) arglist

		_ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce condExprs loopVars loopWrites nonTempVars dependencies accessAnalysis)) codeSeg)	

analyseAccess :: String -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarAccessAnalysis -> Expr Anno -> AnalysisInfo
analyseAccess comment loopVars loopWrites nonTempVars accessAnalysis expr = (unusedIterMap, [],[],[]) --(errors, [],[],[])
								where
									operands = case fnCall of
											True ->	extractContainedVars expr
											False -> extractOperands expr
									writtenOperands = filter (usesVarName_list loopWrites) operands
									fnCall = isFunctionCall accessAnalysis expr
									nonTempWrittenOperands = filter(usesVarName_list nonTempVars) writtenOperands
									
									badExprs =  filter (\item -> listSubtract loopVars (foldl (\accum item -> accum ++ extractVarNames item) [] (extractContainedVars item)) /= []) nonTempWrittenOperands
									badExprStrs = map (\item -> errorLocationFormatting (srcSpan item) ++ outputTab ++ outputExprFormatting item) badExprs

									unusedIterMap = analyseIteratorUse_list nonTempWrittenOperands loopVars comment

									--errorMap = if (badExprStrs == []) 
									--			then nullAnno
									--			else 
									--				DMap.insert (outputTab ++ "Cannot map: Non temporary, write variables accessed without full use of loop iterator:\n") badExprStrs nullAnno

analyseIteratorUse_list :: [Expr Anno] -> [VarName Anno] -> String  -> Anno -- IteratorUseMap
analyseIteratorUse_list nonTempWrittenOperands loopVars comment = foldl (analyseIteratorUse_single nonTempWrittenOperands comment) DMap.empty loopVars

analyseIteratorUse_single :: [Expr Anno] -> String -> Anno -> VarName Anno -> Anno
analyseIteratorUse_single nonTempWrittenOperands comment accumAnno loopVar = resultantMap
								where
									debugMap = foldl (\accum item -> DMap.insert (outputExprFormatting item) (map (outputExprFormatting) (extractContainedVars item)) accum) nullAnno nonTempWrittenOperands
									--offendingExprs = filter (\x -> not (usesVarName loopVar x)) nonTempWrittenOperands
									offendingExprs = filter (\item -> not (elem loopVar (foldl (\accum item -> accum ++ extractVarNames item) [] (extractContainedVars item)))) nonTempWrittenOperands
									offendingExprsStrs = map (\item -> errorLocationFormatting (srcSpan item) ++ outputTab ++ outputExprFormatting item) offendingExprs

									loopVarStr = varnameStr loopVar
									resultantMap = if (offendingExprs == []) 
												then accumAnno
												else 
													DMap.insert (outputTab ++ comment ++ "Non temporary, write variables accessed without use of loop iterator \"" ++ loopVarStr ++ "\":\n") offendingExprsStrs accumAnno
									nonTempWrittenOperandsStrs = map (\item -> errorLocationFormatting (srcSpan item) ++ outputTab ++ outputExprFormatting item) nonTempWrittenOperands

--analyseIteratorUseReduce_list :: [Expr Anno] -> [VarName Anno] -> Anno -- IteratorUseMap
--analyseIteratorUseReduce_list nonTempWrittenOperands loopVars = foldl (analyseIteratorUseReduce_single nonTempWrittenOperands) DMap.empty loopVars

--analyseIteratorUseReduce_single :: [Expr Anno] -> Anno -> VarName Anno -> Anno
--analyseIteratorUseReduce_single nonTempWrittenOperands accumAnno loopVar = resultantMap
--								where
--									debugMap = foldl (\accum item -> DMap.insert (outputExprFormatting item) (map (outputExprFormatting) (extractContainedVars item)) accum) nullAnno nonTempWrittenOperands
--									--offendingExprs = filter (\x -> not (usesVarName loopVar x)) nonTempWrittenOperands
--									offendingExprs = filter (\item -> not (elem loopVar (foldl (\accum item -> accum ++ extractVarNames item) [] (extractContainedVars item)))) nonTempWrittenOperands
--									offendingExprsStrs = map (\item -> errorLocationFormatting (srcSpan item) ++ outputTab ++ outputExprFormatting item) offendingExprs

--									loopVarStr = varnameStr loopVar
--									resultantMap = if (offendingExprs == []) 
--												then accumAnno
--												else 
--													DMap.insert (outputTab ++ "Cannot map: Non temporary, write variables accessed without use of loop iterator \"" ++ loopVarStr ++ "\":\n") offendingExprsStrs accumAnno
--									nonTempWrittenOperandsStrs = map (\item -> errorLocationFormatting (srcSpan item) ++ outputTab ++ outputExprFormatting item) nonTempWrittenOperands

--	Function checks whether the primary in a reduction assignmnet is an associative operation. Checks both associative ops and functions.
isAssociativeExpr :: Expr Anno -> Expr Anno -> Bool
isAssociativeExpr assignee assignment = case assignment of
							(Bin _ _ op expr1 expr2) -> associativeOp
							_ -> associativeFunc
						where 
							primaryOp = extractPrimaryReductionOp assignee assignment
							primaryFunc = extractPrimaryReductionFunction assignee assignment
							associativeOp = case primaryOp of
												Just oper -> isAssociativeOp oper
												Nothing -> False
							associativeFunc = isAssociativeFunction primaryFunc

isAssociativeOp :: BinOp Anno -> Bool
isAssociativeOp (Plus p) = True
isAssociativeOp (Mul p) = True
isAssociativeOp (Or p) = True
isAssociativeOp _ = False

--	Not yet used. In future the program may be able to detect whether or not a variable is given an appropriate start value for a reduction
opIdentityValue :: BinOp Anno -> Expr Anno
opIdentityValue (Plus p) = Con nullAnno nullSrcSpan "0"
opIdentityValue (Mul p) = Con nullAnno nullSrcSpan "1"
opIdentityValue (Or p) = Con nullAnno nullSrcSpan ".FALSE."
opIdentityValue _ = Null nullAnno nullSrcSpan

isAssociativeFunction :: String -> Bool
isAssociativeFunction fnName = case (map (toLower) fnName) of
								"min" -> True
								"max" -> True
								"amax1" -> True
								"amin1" -> True
								_ -> False