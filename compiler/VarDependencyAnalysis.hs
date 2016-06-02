module VarDependencyAnalysis where

--	The code in this file is used to perform dependency analysis for a block of code. A call to 'analyseDependencies'
--	will produce a set of direct dependencies between variables. A direct dependency is formed when one variable is used in the 
--	calculation of another variable's assignment. It is possible under this scheme for variables to depend upon themselves and this
--	fact is ued by Transformer.hs when looking to determine whether or not a loop represnets a reduction. This module also contains
--	functions to get indirect dependencies and determine whether or not loops exhibit loop carried dependencies.

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as DMap

import LanguageFortranTools
import VarAccessAnalysis

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

--	Type used to colate dependency data between variables within a particular block of code
--										Variable A 		depends on all these expressions
type VarDependencyAnalysis = DMap.Map (VarName Anno) [Expr Anno]

--	Type used to colate all of the indexes at which variables are accessed in a chosen way (read/write).
--	There are two ArrayAccessExpressions passed around during analysis, one that holds all of the indexs
--	of a variable that are written to, and one that tracks where the variable is read. These data
--	structures are used to determine whether loop carried dependencies exist in a particular loop.
--										Variable A 		is accessed at indices
type ArrayAccessExpressions = DMap.Map (VarName Anno) 	[[Expr Anno]]

--	Type used to store all possible tuples of iterator values for nested iterator variables.
data TupleTable = LoopIterRecord (DMap.Map Int TupleTable) 
					| Empty
					deriving (Show, Eq)

analyseDependencies :: Fortran Anno -> VarDependencyAnalysis
analyseDependencies codeSeg = foldl (\accum item -> constructDependencies accum item) DMap.empty assignments
						where
							assignments = everything (++) (mkQ [] extractAssignments) codeSeg

constructDependencies :: VarDependencyAnalysis -> Fortran Anno -> VarDependencyAnalysis
constructDependencies prevAnalysis (Assg _ _ expr1 expr2) = foldl (\accum item -> addDependencies accum item readOperands) prevAnalysis writtenVarNames
							where
								--	As part of Language-Fortran's assignment type, the first expression represents the 
								--	variable being assigned to and the second expression is the thing being assigned
								writtenOperands = filter (isVar) (extractOperands expr1)
								readOperands = filter (isVar) (extractOperands expr2)

								writtenVarNames = foldl (\accum item -> accum ++ extractVarNames item) [] writtenOperands

constructDependencies prevAnalysis _ = prevAnalysis

--addDependencies :: VarDependencyAnalysis -> VarName Anno -> [VarName Anno] -> VarDependencyAnalysis
--	A dependent depends on a dependee. For example
--		A = B + 12
--	A depends on B. A is the dependee, B is the dependent
addDependencies :: VarDependencyAnalysis -> VarName Anno -> [Expr Anno] -> VarDependencyAnalysis
addDependencies prevAnalysis dependent dependees = foldl (\accum item -> addDependency accum dependent item) prevAnalysis dependees

addDependency :: VarDependencyAnalysis -> VarName Anno -> Expr Anno -> VarDependencyAnalysis
addDependency prevAnalysis dependent dependee = appendToMap dependent dependee prevAnalysis

getDirectDependencies :: VarDependencyAnalysis -> VarName Anno -> [Expr Anno]
getDirectDependencies analysis queryVarname = DMap.findWithDefault [] queryVarname analysis

getIndirectDependencies :: VarDependencyAnalysis -> VarName Anno -> [Expr Anno]
getIndirectDependencies analysis queryVarname = getIndirectDependencies' analysis queryVarname []

getIndirectDependencies' :: VarDependencyAnalysis -> VarName Anno -> [Expr Anno] -> [Expr Anno]
getIndirectDependencies' analysis queryVarname previouslyProcessed = foldl (\accum item -> accum ++ getIndirectDependencies' analysis (head $ extractVarNames item)  newProcessed) newDependencies newDependencies
											where
												newDependencies = listSubtract (getDirectDependencies analysis queryVarname) previouslyProcessed
												newProcessed = newDependencies ++ previouslyProcessed

isDirectlyDependentOn :: VarDependencyAnalysis -> VarName Anno -> Expr Anno -> Bool
isDirectlyDependentOn analysis potDependent potDependee = elem potDependee dependencies
										where
											dependencies = getDirectDependencies analysis potDependent 

isIndirectlyDependentOn' :: VarDependencyAnalysis -> VarName Anno -> Expr Anno -> [Expr Anno] -> Bool
isIndirectlyDependentOn' analysis potDependent potDependee previouslyProcessed 	|	isDirectlyDependentOn analysis potDependent potDependee = True
																				|	otherwise = foldl (||) False $ map (\x -> isIndirectlyDependentOn' analysis (head $ extractVarNames x) potDependee newProcessed) newDependencies
																						where
																							newDependencies = listSubtract (getDirectDependencies analysis potDependent) previouslyProcessed
																							newProcessed = newDependencies ++ previouslyProcessed

isIndirectlyDependentOn :: VarDependencyAnalysis -> VarName Anno -> Expr Anno -> Bool
isIndirectlyDependentOn analysis potDependent potDependee	|	isDirectlyDependentOn analysis potDependent potDependee = True
															|	otherwise = foldl (||) False $ map (\x -> isIndirectlyDependentOn' analysis (head $ extractVarNames x) potDependee []) dependencies

																	where 
																		dependencies = getDirectDependencies analysis potDependent 

--	Given an AST representing a loop, this function will return a tuple containg a bool signifiying whether a loop carried dependency is possible
--	and a list of pairs of expressions that cause the dependency. The process is as follows:
--		+	Extract all assignments in the loop in question
--		+	Use the assignments to build up two data structures that pair variables with the array index
--			expressions that are used to WRITE to the variable and to READ from the variable.
--		+	Build a 'loopIterTable' which is a data structure representing all of the possible combinations
--			of loop iterator values for the particular loop.
--		+	For every var that is written to, compare with any reads to that var. Iterate through all of
--			the possible loop iterator values and evaluate the index positions that the read and the write
--			index expressions boil down to. Keep track of all index values used to READ the var and all 
--			index values used to WRITE to the var and make sure that if a WRITE never occurs to a place
--			that has PREVIOUSLY been read (not in this iteration) and a READ never occurs to a place that
--			has been PREVIOUSLY written to.
--		+	Index expressions that cause dependencies are passed back to eventually be returned to the
--			caller.
loopCarriedDependencyCheck :: Fortran Anno -> (Bool, [(Expr Anno, Expr Anno)])
loopCarriedDependencyCheck codeSeg = (checkFailure, offendingExprs)
			where
				assignments = everything (++) (mkQ [] extractAssignments) codeSeg
				(reads, writes) = foldl' (extractArrayIndexReadWrite_foldl) (DMap.empty, DMap.empty) assignments
				(loopIterTable_maybe, loopVars, str) = constructLoopIterTable (Just(Empty)) [] codeSeg

				(loopIterTable_successfull, loopIterTable) = case loopIterTable_maybe of 
								Nothing -> (False, Empty)
								Just a -> (True, a)

				writtenVars = DMap.keys writes
				varChecks = map (loopCarriedDependency_varCheck loopIterTable loopVars (reads, writes)) writtenVars
				offendingExprs = foldl (++) [] (map (loopCarriedDependency_varCheck loopIterTable loopVars (reads, writes)) writtenVars)
				checkFailure = (not loopIterTable_successfull) || offendingExprs /= []

--	Given an AST representing a loop, this function will return a tuple containg a bool signifiying whether a loop carried dependency is possible
--	and a list of pairs of expressions that cause the dependency. This version is different from the one above in that this one deals with
--	parallel loops that are nested in iterative loops and require values for loop iterator(s) of the outer loop(s). Works in the same way as above,
--	except only reads/writes in the parallel loop(s) are considered but loop iterator values for the iterating loops are included during the
--	analysis.
loopCarriedDependencyCheck_iterative :: Fortran Anno ->  Fortran Anno -> (Bool, [(Expr Anno, Expr Anno)])
loopCarriedDependencyCheck_iterative iteratingCodeSeg parallelCodeSeg = (checkFailure, if loopIterTable_successfull then offendingExprs else [])
			where
				assignments = everything (++) (mkQ [] extractAssignments) parallelCodeSeg
				(reads, writes) = foldl' (extractArrayIndexReadWrite_foldl) (DMap.empty, DMap.empty) assignments
				(loopIterTable_maybe, loopVars, str) = constructLoopIterTable (Just(Empty)) [] iteratingCodeSeg
				loopVars_parallel = extractLoopVars parallelCodeSeg

				loopVars_iter = listSubtract loopVars loopVars_parallel
				loopIterTableIterations = loopCarriedDependency_iterative_prepareIterTable loopVars_iter loopIterTable

				(loopIterTable_successfull, loopIterTable) = case loopIterTable_maybe of 
												Nothing -> (False, Empty)
												Just a -> (True, a)

				writtenVars = DMap.keys writes
				varChecks = map (loopCarriedDependency_varCheck loopIterTable loopVars (reads, writes)) writtenVars
				offendingExprs = foldl (\accum loopIT -> listConcatUnique accum (foldl (++) [] (map (loopCarriedDependency_varCheck loopIT loopVars_parallel (reads, writes)) writtenVars))) [] loopIterTableIterations

				checkFailure = (not loopIterTable_successfull) || offendingExprs /= []

loopCarriedDependency_iterative_prepareIterTable :: [VarName Anno] -> TupleTable -> [TupleTable]
loopCarriedDependency_iterative_prepareIterTable [] iterTable = [iterTable]
loopCarriedDependency_iterative_prepareIterTable loopVars (LoopIterRecord iterTable) = selections
			where
				currentLoopVar = head loopVars
				allowedValues = DMap.keys iterTable
				selections = foldl (\accum item -> accum ++ (loopCarriedDependency_iterative_prepareIterTable (tail loopVars) (DMap.findWithDefault Empty item iterTable))) [] allowedValues
				
loopCarriedDependency_varCheck :: TupleTable -> [VarName Anno] -> (ArrayAccessExpressions, ArrayAccessExpressions) -> VarName Anno -> [(Expr Anno, Expr Anno)]
loopCarriedDependency_varCheck loopIterTable loopVars (reads, writes) var = offendingExprs
			where
				writtenAccesses = DMap.findWithDefault [] var writes
				readsAccesses = DMap.findWithDefault [] var reads

				offendingIndexPairs = foldl (loopCarriedDependency_writtenExprCheck loopIterTable loopVars readsAccesses) [] writtenAccesses
				offendingExprs = map (\(read, written) -> (generateArrayVar var read, generateArrayVar var written)) offendingIndexPairs


loopCarriedDependency_writtenExprCheck :: TupleTable -> [VarName Anno] -> [[Expr Anno]] -> [([Expr Anno], [Expr Anno])] -> [Expr Anno] -> [([Expr Anno], [Expr Anno])]
loopCarriedDependency_writtenExprCheck loopIterTable loopVars readExprs oldOffendingExprs writtenExpr = oldOffendingExprs ++ dependencyPairs
			where
				offendingReads = foldl (loopCarriedDependency_readExprCheck loopIterTable loopVars writtenExpr) [] readExprs
				dependencyPairs = map (\x -> (x, writtenExpr)) offendingReads

loopCarriedDependency_readExprCheck :: TupleTable -> [VarName Anno] -> [Expr Anno] -> [[Expr Anno]] -> [Expr Anno] -> [[Expr Anno]]
loopCarriedDependency_readExprCheck loopIterTable loopVars writtenIndexExprs oldOffendingExprs readIndexExprs = result
			where
				(offend_bool, reads, writes) = loopCarriedDependency_evaluatePossibleIndices loopIterTable loopVars readIndexExprs writtenIndexExprs (False, Empty, Empty) DMap.empty
				result = if offend_bool then oldOffendingExprs ++ [readIndexExprs] else oldOffendingExprs

--	The evalation of the possible index values is performed here and the loop dependency analysis checks performed.
--	ARGUMENTS (in order)
--		TupleTable ---------------------Loop iterator table that is traversed as the recursion goes deeper. A value for the iterator at this 'level' of the table is selected
--										and inserted into the 'value table' so that when the indices in question are evaluated, the value for that iterator variable is available.
-- 		[VarName Anno] -----------------Loop variables list used to determine which loop iterator variable is the current one at this level of recursion. This allows for the correct
--										variable name to be assigned a value in the value table from the current level of the loop iterator table.
--	 	[Expr Anno] --------------------The indices that the current variable is READ at.
--	 	[Expr Anno] --------------------The indices that the current variable is WRITTEN at.
--	 	(Bool, TupleTable, TupleTable) -Since this is a fold, this is the accumulator. Bool is whether a dependency exists, the TupleTables are all of the resolved/evaluated index
--										positions for all of the READS and WRITES (respecitvely) that have been calculated so far.
--	 	ValueTable ---------------------A set of assigned values for the loop iterator variables that allows for indices that use loop iterators to be evaluated.
--	 	(Bool, TupleTable, TupleTable) -Return type. Bool is whether a dependency exists, the TupleTables are all of the resolved/evaluated index
--										positions for all of the READS and WRITES (respecitvely) that have been calculated so far.
loopCarriedDependency_evaluatePossibleIndices :: TupleTable -> [VarName Anno] -> [Expr Anno] -> [Expr Anno] -> (Bool, TupleTable, TupleTable) -> ValueTable -> (Bool, TupleTable, TupleTable)
loopCarriedDependency_evaluatePossibleIndices Empty loopVars exprs1 exprs2 (prevCheck, prevReads, prevWrites) valueTable = (prevCheck || depExistsBool, newReads, newWrites)
			where
				reads_eval = map (evaluateExpr valueTable) exprs1
				writes_eval = map (evaluateExpr valueTable) exprs2

				(readsEvaluated, reads_fromMaybe) = foldl (convertFromMaybe_foldl) (True, []) reads_eval
				(writesEvaluated, writes_fromMaybe) = foldl (convertFromMaybe_foldl) (True, []) writes_eval

				reads_fromMaybe_int = (map (round) reads_fromMaybe)
				writes_fromMaybe_int = (map (round) writes_fromMaybe)

				readPreviouslyWritten = case lookupTupleTable reads_fromMaybe_int prevWrites of 
					Just a -> True
					Nothing -> False
				writePreviouslyRead = case lookupTupleTable writes_fromMaybe_int prevReads of 
					Just a -> True
					Nothing -> False

				newReads = insertIntoTupleTable reads_fromMaybe_int prevReads
				newWrites = insertIntoTupleTable writes_fromMaybe_int prevWrites
				depExistsBool = readPreviouslyWritten || writePreviouslyRead || (not readsEvaluated) || (not writesEvaluated)

loopCarriedDependency_evaluatePossibleIndices (LoopIterRecord iterTable) loopVars exprs1 exprs2 previousAnalysis valueTable = analysis
			where
				allowedValues = DMap.keys iterTable
				valueTableIterations = map (\x -> addToValueTable (chosenVar) (fromIntegral x :: Float) valueTable) allowedValues
				accessIterTable = (\x -> DMap.findWithDefault Empty x iterTable)

				chosenVar = head loopVars
				newLoopVars = tail loopVars

				exprs1_chosenVarMask = maskOnVarNameUsage chosenVar exprs1
				exprs2_chosenVarMask = maskOnVarNameUsage chosenVar exprs2

				--	Check to see whether optimisation can be performed that essentially skips a whole loop iterator stage.
				--	IDEA - DON'T DO THIS EVERY LOOP, DO A CHECK AT THE TOP LEVEL AND COLLAPSE LOOP ITER TABLE UP THERE. MAKE FASTER - YES?
				varAffectsOutcome = exprs1_chosenVarMask /= exprs2_chosenVarMask

				analysis = if varAffectsOutcome 
							then foldl (\accum (table, value) -> loopCarriedDependency_evaluatePossibleIndices (accessIterTable value) newLoopVars exprs1 exprs2 accum table) previousAnalysis (zip valueTableIterations allowedValues) 
							else (
								if valueTableIterations /= [] 
									then (\(table, value) -> loopCarriedDependency_evaluatePossibleIndices (collapseIterTable (LoopIterRecord iterTable)) newLoopVars exprs1 exprs2 previousAnalysis table) (head (zip valueTableIterations allowedValues)) 
									else previousAnalysis
								)

convertFromMaybe_foldl :: (Bool, [a]) -> Maybe(a) -> (Bool, [a])
convertFromMaybe_foldl (prevCheck, prevList) (Just(item)) = (prevCheck && True, prevList++[item])
convertFromMaybe_foldl (prevCheck, prevList) Nothing = (False, prevList)

maskOnVarNameUsage :: VarName Anno -> [Expr Anno] -> [Expr Anno]
maskOnVarNameUsage chosenVar exprs = map (\x -> if varNameUsed chosenVar x then applyGeneratedSrcSpans x else Null nullAnno nullSrcSpan) exprs

varNameUsed :: VarName Anno -> Expr Anno -> Bool
varNameUsed chosenVar expr = foldl (\accum item -> accum || (if isVar item then (head (extractVarNames item)) == chosenVar else False)) False (extractOperands expr)

extractArrayIndexReadWrite_foldl :: (ArrayAccessExpressions, ArrayAccessExpressions) -> Fortran Anno -> (ArrayAccessExpressions, ArrayAccessExpressions)
extractArrayIndexReadWrite_foldl (reads, writes) (Assg _ _ expr1 expr2) = (newReads, newWrites)
			where
				readExpr_operands = filter (isVar) (extractOperands expr2)
				readExpr_varNames = map (\x -> head $ extractVarNames x) readExpr_operands
				readExpr_indexExprs = map (extractContainedVars) readExpr_operands

				writtenExpr_varName = head $ extractVarNames expr1
				writtenExpr_indexExprs = extractContainedVars expr1

				newReads = -- if writtenExpr_indexExprs /= []
								-- then 
									foldl (\accum (var, exprs) -> if exprs /= [] then appendToMap var exprs accum else accum) reads (zip readExpr_varNames readExpr_indexExprs)
								-- else reads
				newWrites = if writtenExpr_indexExprs /= []
								then appendToMap writtenExpr_varName writtenExpr_indexExprs writes
								else writes
extractArrayIndexReadWrite_foldl (reads, writes) _ = (reads, writes)

constructLoopIterTable :: Maybe(TupleTable) -> [VarName Anno] -> Fortran Anno -> (Maybe(TupleTable), [VarName Anno], String)
constructLoopIterTable Nothing loopVars _ = (Nothing, loopVars, "")
constructLoopIterTable (Just (oldTable)) loopVars (For _ _ var e1 e2 e3 fortran) = childrenTable
			where
				childrenTable = constructLoopIterTable newLoopIterTable (loopVars ++ [var]) fortran
				newLoopIterTable = extendLoopIterTable oldTable DMap.empty loopVars e1 e2 e3
constructLoopIterTable (Just (oldTable)) loopVars (OpenCLMap _ _ _ _ loopBoundsList fortran) = childrenTable
			where
				(var, e1, e2, e3) = head loopBoundsList
				childrenTable = constructLoopIterTable newLoopIterTable (loopVars ++ [var]) fortran
				newLoopIterTable = extendLoopIterTable oldTable DMap.empty loopVars e1 e2 e3
constructLoopIterTable (Just (oldTable)) loopVars (OpenCLReduce _ _ _ _ loopBoundsList _ fortran) = childrenTable
			where
				(var, e1, e2, e3) = head loopBoundsList
				childrenTable = constructLoopIterTable newLoopIterTable (loopVars ++ [var]) fortran
				newLoopIterTable = extendLoopIterTable oldTable DMap.empty loopVars e1 e2 e3
constructLoopIterTable (Just (oldTable)) loopVars codeSeg = (newTable, newLoopVars, "")
			where
				childrenAnalysis = gmapQ (mkQ (Just(Empty), [], "") (constructLoopIterTable (Just oldTable) (loopVars))) codeSeg
				nonEmptyTables = filter (\(table, _, _) -> tupleTableNotEmpty (fromMaybe Empty table)) childrenAnalysis
				(newTable, newLoopVars, newString) = if (length nonEmptyTables) == 0 then 
																		(Just oldTable, loopVars, "")  else head nonEmptyTables

insertIntoTupleTable :: [Int] -> TupleTable -> TupleTable
insertIntoTupleTable [] tupleTable = tupleTable
insertIntoTupleTable (indices) Empty = createTupleTable indices
insertIntoTupleTable (indices) (LoopIterRecord table) = case DMap.lookup (head indices) table of 
															Just subTable -> LoopIterRecord (DMap.insert (head indices) (insertIntoTupleTable (tail indices) subTable) table)
															Nothing -> LoopIterRecord (DMap.insert (head indices) (createTupleTable (tail indices)) table)

collapseIterTable :: TupleTable -> TupleTable
collapseIterTable (LoopIterRecord iterTable) = foldl (\accum item -> joinTupleTable accum item) Empty subTables
			where
				allowedValues = DMap.keys iterTable
				subTables = map (\x -> DMap.findWithDefault Empty x iterTable) allowedValues

joinTupleTable :: TupleTable -> TupleTable -> TupleTable
joinTupleTable Empty Empty = Empty
joinTupleTable table1 table2 = foldl (joinTupleTable_foldl table1 table2) newTable allowedValues
			where
				allowedValues = case table1 of
									LoopIterRecord a -> case table2 of 
															Empty -> DMap.keys a
															LoopIterRecord b -> listConcatUnique (DMap.keys a) (DMap.keys b)
									Empty -> 			case table2 of 
															Empty -> []
															LoopIterRecord b -> DMap.keys b
				newTable = LoopIterRecord DMap.empty

joinTupleTable_foldl :: TupleTable -> TupleTable -> TupleTable -> Int -> TupleTable
joinTupleTable_foldl (LoopIterRecord table1) (LoopIterRecord table2) (LoopIterRecord newTable) value = LoopIterRecord (DMap.insert value joinedTable newTable)
			where
				subTable1 = DMap.findWithDefault Empty value table1
				subTable2 = DMap.findWithDefault Empty value table2
				joinedTable = joinTupleTable subTable1 subTable2
joinTupleTable_foldl (LoopIterRecord table1) Empty (LoopIterRecord newTable) value = LoopIterRecord (DMap.insert value (subTable1) newTable)
			where
				subTable1 = DMap.findWithDefault Empty value table1
joinTupleTable_foldl Empty (LoopIterRecord table2) (LoopIterRecord newTable) value = LoopIterRecord (DMap.insert value (subTable2) newTable)
			where
				subTable2 = DMap.findWithDefault Empty value table2
joinTupleTable_foldl Empty Empty (LoopIterRecord newTable) value = LoopIterRecord (DMap.insert value Empty newTable)


createTupleTable :: [Int] -> TupleTable
createTupleTable [] = Empty
createTupleTable indices = LoopIterRecord (DMap.insert (head indices) (createTupleTable (tail indices)) DMap.empty)

lookupTupleTable :: [Int] -> TupleTable -> Maybe(TupleTable)
lookupTupleTable [index] (LoopIterRecord table) = DMap.lookup index table
lookupTupleTable (index:indices) (LoopIterRecord table) = lookupTupleTable indices (DMap.findWithDefault Empty index table)
lookupTupleTable _ _ = Nothing

tupleTableNotEmpty :: TupleTable -> Bool
tupleTableNotEmpty Empty = False
tupleTableNotEmpty _ = True

extendLoopIterTable :: TupleTable -> ValueTable -> [VarName Anno] -> Expr Anno -> Expr Anno -> Expr Anno -> Maybe(TupleTable)
extendLoopIterTable oldTable valueTable ([]) startExpr endExpr stepExpr = case range of
																			[] -> Nothing
																			_ -> Just (addRangeToIterTable oldTable range)
		where
			range = evaluateRange valueTable startExpr endExpr stepExpr
extendLoopIterTable Empty valueTable _ startExpr endExpr stepExpr = case range of
																			[] -> Nothing
																			_ -> Just (addRangeToIterTable Empty range)
		where
			range = evaluateRange valueTable startExpr endExpr stepExpr
extendLoopIterTable oldTable valueTable loopVars startExpr endExpr stepExpr = foldl 
																				(extendLoopIterTableWithValues_foldl valueTable loopVars startExpr endExpr stepExpr)
																				(Just oldTable) 
																				allowedValues
		where
			allowedValues = case oldTable of
								LoopIterRecord a -> DMap.keys a
								_ -> []

extendLoopIterTableWithValues_foldl :: ValueTable -> [VarName Anno] -> Expr Anno -> Expr Anno -> Expr Anno -> Maybe(TupleTable) -> Int -> Maybe(TupleTable)
extendLoopIterTableWithValues_foldl valueTable loopVars startExpr endExpr stepExpr Nothing chosenValue = Nothing
extendLoopIterTableWithValues_foldl valueTable loopVars startExpr endExpr stepExpr (Just (LoopIterRecord oldRecord)) chosenValue = case newSubTable of
																																	Just newT -> Just (LoopIterRecord (DMap.insert chosenValue newT oldRecord))
																																	Nothing -> Nothing
		where
			oldSubTable = DMap.findWithDefault Empty chosenValue oldRecord
			newSubTable = extendLoopIterTable oldSubTable (addToValueTable (head loopVars) (fromIntegral chosenValue :: Float) valueTable) newLoopVars startExpr endExpr stepExpr
			newLoopVars = tail loopVars

addRangeToIterTable :: TupleTable -> [Float] -> TupleTable
addRangeToIterTable oldTable range = LoopIterRecord (foldl (\accum key -> DMap.insert (round key) Empty accum) oldRecord range)
		where
			oldRecord = case oldTable of
							Empty -> DMap.empty
							LoopIterRecord a -> a