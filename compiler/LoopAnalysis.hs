module LoopAnalysis where

--	This module holds functions that perform the top level of analysis. They look to see whether each of the loops in the source
--	can be parallelised or not. Calls to 'VarAccessAnalysis' and 'VarDependencyAnalysis' are made from here and the information
--	produce by those other modules is used to deteremine whether the conditions for parallelism are met. This module also handles
--	producing parallelism errors that are later attached to AST nodes. 

-- 	STRATEGY FOR NEW LOOP CARRIED DEPENDENCY CHECK
-- 	Looking to build up a table of all of the possible combinations of loopVar/loop iterator values. This will only be really useful
--	when constant folding works. For now, we use macros to hardcode the values for loop bounds. With the table, we check for loop 
--	carried dependencies by looking at all accesses of a particular array within a particular loop. Given the set of distinct
--	expressions that define array indices for READs and and the distinct set of expressions that define array indices for WRITES,
--	make sure that no READ WRITE pair can exist at the same time in the table of possible combinations of loop iterator values. 
-- 
--	For example:
-- 		for j in range(0,10,1)
-- 			for k in range(i%2,10,2)
-- 				p(j,k) = p(j,k-1) + 12
-- 
-- 	There cannot exist a situation where (j,k) exists in the table and (j,k-1) exists therefore there is no loop carried dependency
--
--	Building and extending the table will be performed when a new for loop is encountered. This is going to involve adding more
--	arguments to some already rather complex functions and adding functionality to build this table. Further complications occur
--	when loop bounds are defined in terms of outer loop iterators. The table itself will be a map of maps of maps of maps... to
--	the power of the current loop nest depth. For example, the table will be a map of maps of maps for a triple nested loop
--	iterating over i, j and k where each level of the map corresponds to a loop iterator variable. For example, if the value (1,1,4)
-- 	is allowed for (i,j,k) then table[1][1][4] exists and contains an empty map. If the value (1,1,5) is not allowed for (i,j,k) then
--	table[1][1][5] will not exist.

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

-- data LoopIterTable = LoopIterTable (DMap.Map Int (LoopIterTable))
-- data LoopIterTable = LoopIterTable (DMap.Map Int (LoopIterTable))

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
											DMap.insert (outputTab ++ "Cannot map: Loop carried dependency on " ++ outputExprFormatting expr1 ++ ":\n")
												(map (\item -> errorLocationFormatting (srcSpan item) ++ outputTab ++ outputExprFormatting item) loopCarriedDependencyList)
												DMap.empty
											else DMap.empty

		For _ _ var _ _ _ _ -> foldl combineAnalysisInfo analysisInfoBaseCase childrenAnalysis
						where
							childrenAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_map (loopVars ++ [var]) loopWrites nonTempVars accessAnalysis dependencies)) codeSeg)
		Call _ srcspan expr arglist -> (errorMap_call, [], [], argExprs)
						where
							errorMap_call = DMap.insert (outputTab ++ "Cannot map: Call to external function:\n")
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
									|| (foldl (||) False $ map (\x -> isIndirectlyDependentOn dependencies (head $ extractVarNames x) x) writtenExprs)
				
				expr1Analysis = (analyseAccess "Cannot reduce: " loopVars loopWrites nonTempVars accessAnalysis expr1)
				expr2Analysis = (analyseAccess "Cannot reduce: " loopVars loopWrites nonTempVars accessAnalysis expr2)
				doesNotUseFullLoopVar = (\(errorMap, _, _, _) -> errorMap == nullAnno) expr1Analysis

				potentialReductionVar = isNonTempAssignment && (dependsOnSelf) -- && doesNotUseFullLoopVar

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
												errorMap3
											else errorMap3

		Call _ srcspan expr arglist -> (errorMap_call, [], [], argExprs)
			where
				errorMap_call = DMap.insert (outputTab ++ "Cannot reduce: Call to external function:\n")
												[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr]
												DMap.empty
				argExprs = everything (++) (mkQ [] extractExpr_list) arglist

		_ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce condExprs loopVars loopWrites nonTempVars dependencies accessAnalysis)) codeSeg)	

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

									loopVarStr = varnameStr loopVar
									resultantMap = if (offendingExprs == []) 
												then accumAnno
												else 
													DMap.insert (outputTab ++ comment ++ "Non temporary, write variables accessed without use of loop iterator \"" ++ loopVarStr ++ "\":\n") offendingExprsStrs accumAnno
									nonTempWrittenOperandsStrs = map (\item -> errorLocationFormatting (srcSpan item) ++ outputTab ++ outputExprFormatting item) nonTempWrittenOperands

-- extendLoopIterTable :: [VarName Anno] -> LoopIterTable -> Expr Anno -> Expr Anno -> Expr Anno -> LoopIterTable
-- extendLoopIterTable loopVars oldTable startExpr endExpr stepExpr  =  DMap.empty
-- 																	-- | outermostValues == [] = LoopIterTable DMap.empty
-- 																	-- | otherwise = LoopIterTable DMap.empty -- foldl (\accum key -> extendLoopIterTable loopVars (DMap.findWithDefault (error "extendLoopIterTable: findWithDefault") key accum) startExpr endExpr stepExpr) oldTable outermostValues
-- 				where
-- 					outermostValues = DMap.toList oldTable

exprEvaluator :: Expr Anno -> Int
exprEvaluator (Bin _ _ binOp expr1 expr2) = case binOp of
												Plus _ -> exprEvaluator expr1 + exprEvaluator expr2
												Minus _ -> exprEvaluator expr1 - exprEvaluator expr2
												Mul _ -> exprEvaluator expr1 * exprEvaluator expr2
												Div _ -> quot (exprEvaluator expr1) (exprEvaluator expr2)
												Power _ -> (exprEvaluator expr1) ^ (exprEvaluator expr2)
												_ -> 0
exprEvaluator (Con _ _ str) = read str :: Int
exprEvaluator _ = 0

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