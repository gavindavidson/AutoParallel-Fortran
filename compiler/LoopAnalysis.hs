module LoopAnalysis where

--	This module holds functions that perform the top level of analysis. They look to see whether each of the loops in the source
--	can be parallelised or not. Calls to 'VarAccessAnalysis' and 'VarDependencyAnalysis' are made from here and the information
--	produce by those other modules is used to deteremine whether the conditions for parallelism are met. This module also handles
--	producing parallelism errors that are later attached to AST nodes. 

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
type AnalysisInfo = 	(Anno, 		[Expr Anno], 		[Expr Anno], 		[Expr Anno])

analysisInfoBaseCase :: AnalysisInfo
analysisInfoBaseCase = (nullAnno,[],[],[])

combineAnalysisInfo :: AnalysisInfo -> AnalysisInfo -> AnalysisInfo
combineAnalysisInfo accum item = (combineMaps accumErrors itemErrors, accumReductionVars ++ itemReductionVars, accumReads ++ itemReads, accumWrites ++ itemWrites)
								where
									(accumErrors, accumReductionVars, accumReads, accumWrites) = accum
									(itemErrors, itemReductionVars, itemReads, itemWrites)	 = item
									
getErrorAnnotations :: AnalysisInfo -> Anno
getErrorAnnotations (errors, _, _, _) = errors

getReductionVars :: AnalysisInfo -> [Expr Anno]
getReductionVars (_, reductionVars, _, _) = reductionVars

getReads :: AnalysisInfo -> [Expr Anno]
getReads (_, _, reads, _) = reads

getWrites :: AnalysisInfo -> [Expr Anno]
getWrites (_, _, _, writes) = writes

--	Function takes a list of loop variables and a possible parallel loop's AST and returns a string that details the reasons why the loop
--	cannot be mapped. If the returned string is empty, the loop represents a possible parallel map
analyseLoop_map :: String -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarAccessAnalysis -> VarDependencyAnalysis -> Fortran Anno -> AnalysisInfo
analyseLoop_map comment loopVars loopWrites nonTempVars accessAnalysis dependencies codeSeg = case codeSeg of
		If _ _ expr _ elifList maybeElse -> foldl combineAnalysisInfo analysisInfoBaseCase (generalAnalysis ++ elifAnalysis_fortran ++ elifAnalysis_readExprs ++ [elseAnalysis])
						where
							generalAnalysis = gmapQ (mkQ analysisInfoBaseCase (analyseLoop_map comment   (loopVars) loopWrites nonTempVars accessAnalysis dependencies)) codeSeg
							elifAnalysis_fortran = map (\(_, elif_fortran) -> analyseLoop_map comment   (loopVars) loopWrites nonTempVars accessAnalysis dependencies elif_fortran) elifList
							elifAnalysis_readExprs = map (\(elif_expr, _) -> (nullAnno,[],extractOperands elif_expr,[])) elifList
							elseAnalysis = case maybeElse of
												Just else_fortran ->  analyseLoop_map comment   (loopVars) loopWrites nonTempVars accessAnalysis dependencies else_fortran
												Nothing -> analysisInfoBaseCase
		Assg _ srcspan expr1 expr2 -> foldl (combineAnalysisInfo) analysisInfoBaseCase [expr1Analysis, --expr2Analysis, 
																								(DMap.empty,[],expr2Operands,[expr1])] -- combineAnalysisInfo (combineAnalysisInfo expr1Analysis expr2Analysis) (nullAnno,[],expr2Operands,[expr1])
						where 																						-- loopCarriedDependencyErrorMap
							expr1Analysis = (analyseAccess comment loopVars loopWrites nonTempVars accessAnalysis expr1)
							expr2Analysis = (analyseAccess comment loopVars loopWrites nonTempVars accessAnalysis expr2)
							expr2Operands = extractOperands expr2

		For _ _ var e1 e2 e3 _ -> foldl combineAnalysisInfo analysisInfoBaseCase childrenAnalysis -- ++ [(DMap.insert ("LOOPITERTABLE:\n") [ show (loopVars ++ [var]) ++ "\n" ++ (show newLoopIterTable) ++ "\n"] DMap.empty, [],[],[])])
						where
							childrenAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_map comment   (loopVars ++ [var]) loopWrites nonTempVars accessAnalysis dependencies)) codeSeg)
		Call _ srcspan expr arglist -> (errorMap_call, [], [], argExprs)
						where
							errorMap_call = DMap.insert (outputTab ++ comment ++ "Call to external function:\n")
															[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr]
															DMap.empty
							argExprs = everything (++) (mkQ [] extractExpr_list) arglist
		_ -> foldl combineAnalysisInfo analysisInfoBaseCase (childrenAnalysis ++ nodeAccessAnalysis)
						where
							nodeAccessAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseAccess comment loopVars loopWrites nonTempVars accessAnalysis)) codeSeg)
							childrenAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_map comment   loopVars loopWrites nonTempVars accessAnalysis dependencies)) codeSeg)

--	Function takes a list of loop variables and a possible parallel loop's AST and returns a string that details the reasons why the loop
--	doesn't represent a reduction. If the returned string is empty, the loop represents a possible parallel reduction
analyseLoop_reduce :: String -> [Expr Anno] -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarDependencyAnalysis -> VarAccessAnalysis -> Fortran Anno -> AnalysisInfo
analyseLoop_reduce comment condExprs loopVars loopWrites nonTempVars dependencies accessAnalysis codeSeg = case codeSeg of
		If _ _ expr _ elifList maybeElse -> foldl combineAnalysisInfo analysisInfoBaseCase (generalAnalysis ++ elifAnalysis_fortran ++ elifAnalysis_readExprs ++ [elseAnalysis])
							where
								generalAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce comment (condExprs ++ [expr]) loopVars loopWrites nonTempVars dependencies accessAnalysis)) codeSeg)
								elifAnalysis_fortran = map (\(elif_expr, elif_fortran) -> analyseLoop_reduce comment (condExprs ++ [elif_expr]) loopVars loopWrites nonTempVars dependencies accessAnalysis elif_fortran) elifList
								elifAnalysis_readExprs = map (\(elif_expr, _) -> (nullAnno,[],extractOperands elif_expr,[])) elifList
								elseAnalysis = case maybeElse of
												Just else_fortran -> analyseLoop_reduce comment (condExprs) loopVars loopWrites nonTempVars dependencies accessAnalysis else_fortran
												Nothing -> analysisInfoBaseCase
		For _ _ var e1 e2 e3 _ -> foldl combineAnalysisInfo analysisInfoBaseCase childrenAnalysis -- ++ [(DMap.insert ("LOOPITERTABLE:\n") [ show (loopVars ++ [var]) ++ "\n" ++ (show newLoopIterTable) ++ "\n"] DMap.empty, [],[],[])])
							where
								-- newLoopIterTable = extendLoopIterTable loopIterTable DMap.empty loopVars e1 e2 e3
								childrenAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce comment condExprs (loopVars ++ [var]) loopWrites nonTempVars dependencies accessAnalysis)) codeSeg)
		Assg _ srcspan expr1 expr2 -> 	combineAnalysisInfo
											(errorMap3
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

				dependsOnSelfOnce = length (filter (\item -> applyGeneratedSrcSpans item == applyGeneratedSrcSpans expr1) readExprs) == 1
				-- dependsOnSelfOnce = length (filter (\item -> applyGeneratedSrcSpans item == applyGeneratedSrcSpans expr1) readOperands) == 1

				writtenVarname = head $ foldl (\accum item -> accum ++ extractVarNames item) [] writtenExprs
				readVarnames 	= foldl (\accum item -> accum ++ extractVarNames item) [] readExprs

				isNonTempAssignment = usesVarName_list nonTempVars expr1
				
				referencedCondition = (foldl (||) False $ map (\x -> hasOperand x expr1) condExprs)
				referencedSelf = (hasOperand expr2 expr1)
				associative = isAssociativeExpr expr1 expr2

				dependsOnSelf = referencedSelf || referencedCondition || dependsOnSelfOnce 
									|| (foldl (||) False $ map (\x -> isIndirectlyDependentOn dependencies (head $ extractVarNames x) x) writtenExprs)
				
				expr1Analysis = (analyseAccess comment loopVars loopWrites nonTempVars accessAnalysis expr1)
				expr2Analysis = (analyseAccess comment loopVars loopWrites nonTempVars accessAnalysis expr2)
				doesNotUseFullLoopVar = (\(errorMap, _, _, _) -> errorMap /= nullAnno) expr1Analysis

				potentialReductionVar = isNonTempAssignment && (dependsOnSelf) && doesNotUseFullLoopVar

				errorMap1 = DMap.empty
				errorMap2 = if potentialReductionVar && (not dependsOnSelfOnce) then
											DMap.insert (outputTab ++ comment ++ "Possible reduction variables must only appear once on the right hand side of an assignment:\n")
												[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr1]
												errorMap1
											else errorMap1
				errorMap3 = if dependsOnSelfOnce && potentialReductionVar && (not associative) then
											DMap.insert (outputTab ++ comment ++ "Not associative function:\n")
												[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr2]
												errorMap2
											else errorMap2
				errorMapDebug = DMap.insert (outputTab ++ comment ++ "Debug:\n")
												["potentialReductionVar: " ++ show potentialReductionVar,
												"isNonTempAssignment: " ++ show isNonTempAssignment,
												"dependsOnSelf: " ++ show dependsOnSelf,
												"dependsOnSelfOnce: " ++ show dependsOnSelfOnce,
												"doesNotUseFullLoopVar: " ++ show doesNotUseFullLoopVar
												] 
												errorMap3

		Call _ srcspan expr arglist -> (errorMap_call, [], [], argExprs)
			where
				errorMap_call = DMap.insert (outputTab ++ comment ++ "Call to external function:\n")
												[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr]
												DMap.empty
				argExprs = everything (++) (mkQ [] extractExpr_list) arglist

		_ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce comment condExprs loopVars loopWrites nonTempVars dependencies accessAnalysis)) codeSeg)	

analyseAccess :: String -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarAccessAnalysis -> Expr Anno -> AnalysisInfo
analyseAccess comment loopVars loopWrites nonTempVars accessAnalysis expr = (unusedIterMap, [],[],[])
								where
									operands = case fnCall of
											True ->	extractContainedVars expr
											False -> extractOperands expr
									writtenOperands = filter (usesVarName_list loopWrites) operands
									fnCall = isFunctionCall accessAnalysis expr
									nonTempWrittenOperands = filter(usesVarName_list nonTempVars) writtenOperands

									unusedIterMap = analyseIteratorUse_list nonTempWrittenOperands loopVars comment

analyseIteratorUse_list :: [Expr Anno] -> [VarName Anno] -> String  -> Anno
analyseIteratorUse_list nonTempWrittenOperands loopVars comment = foldl (analyseIteratorUse_single nonTempWrittenOperands comment) DMap.empty loopVars

analyseIteratorUse_single :: [Expr Anno] -> String -> Anno -> VarName Anno -> Anno
analyseIteratorUse_single nonTempWrittenOperands comment accumAnno loopVar = resultantMap
		where
			offendingExprs = filter (\item -> not (elem loopVar (foldl (\accum item -> accum ++ extractVarNames item) [] (extractContainedOperands item) ))) nonTempWrittenOperands
			offendingExprsStrs = map (\item -> errorLocationFormatting (srcSpan item) ++ outputTab ++ outputExprFormatting item) offendingExprs

			loopVarStr = varNameStr loopVar
			resultantMap = if (offendingExprs == []) 
						then accumAnno
						else 
							DMap.insert (outputTab ++ comment ++ "Non temporary, write variables accessed without use of loop iterator \"" ++ loopVarStr ++ "\":\n") offendingExprsStrs accumAnno
			nonTempWrittenOperandsStrs = map (\item -> errorLocationFormatting (srcSpan item) ++ outputTab ++ outputExprFormatting item) nonTempWrittenOperands

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