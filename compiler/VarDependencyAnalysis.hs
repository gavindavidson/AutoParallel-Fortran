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
import TupleTable

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

type LoopStepTable = DMap.Map (VarName Anno) (Float)

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
loopCarriedDependencyCheck :: Fortran Anno -> (Bool, Bool, [(Expr Anno, Expr Anno)])
loopCarriedDependencyCheck codeSeg 	|	simpleFailure = case inDepthFailure of 
															True -> (True, loopIterTable_successfull, offendingExprs ++ simpleOffenders)
															False -> (False, loopIterTable_successfull, [])
									|	otherwise = (False, loopIterTable_successfull, [])
			where
				assignments = everything (++) (mkQ [] extractAssignments) codeSeg
				(reads, writes) = foldl' (extractArrayIndexReadWrite_foldl) (DMap.empty, DMap.empty) assignments
				(loopIterTable_maybe, loopVars, loopStepTable) = constructLoopIterTable (Just(Empty)) DMap.empty [] codeSeg

				(loopIterTable_successfull, loopIterTable) = case loopIterTable_maybe of 
								Nothing -> (False, Empty)
								Just a -> (True, a)

				writtenVars = DMap.keys writes
				offendingExprs = foldl (++) [] (map (loopCarriedDependency_varCheck loopStepTable loopIterTable loopVars (reads, writes)) writtenVars)
				inDepthFailure = (not loopIterTable_successfull) || offendingExprs /= []

				(simpleFailure, simpleOffenders) = simpleLoopCarriedDependencyCheck reads writes

--	Given an AST representing a loop, this function will return a tuple containg a bool signifiying whether a loop carried dependency is possible
--	and a list of pairs of expressions that cause the dependency. This version is different from the one above in that this one deals with
--	parallel loops that are nested in iterative loops and require values for loop iterator(s) of the outer loop(s). Works in the same way as above,
--	except only reads/writes in the parallel loop(s) are considered but loop iterator values for the iterating loops are included during the
--	analysis.
loopCarriedDependencyCheck_iterative :: Fortran Anno ->  Fortran Anno -> (Bool, Bool, [(Expr Anno, Expr Anno)])
loopCarriedDependencyCheck_iterative iteratingCodeSeg parallelCodeSeg 	|	simpleFailure = case inDepthFailure of 
																				True -> (True, loopIterTable_successfull, if loopIterTable_successfull then offendingExprs ++ simpleOffenders else simpleOffenders)
																				False -> (False, loopIterTable_successfull, [])
																		|	otherwise = (False, loopIterTable_successfull, []) 
			where
				assignments = everything (++) (mkQ [] extractAssignments) parallelCodeSeg
				(reads, writes) = foldl' (extractArrayIndexReadWrite_foldl) (DMap.empty, DMap.empty) assignments
				(loopIterTable_maybe, loopVars, loopStepTable) = constructLoopIterTable (Just(Empty)) DMap.empty [] iteratingCodeSeg
				loopVars_parallel = extractLoopVars parallelCodeSeg

				loopVars_iter = listSubtract loopVars loopVars_parallel
				loopIterTableIterations = loopCarriedDependency_iterative_prepareIterTable loopVars_iter loopIterTable

				(loopIterTable_successfull, loopIterTable) = case loopIterTable_maybe of 
												Nothing -> (False, Empty)
												Just a -> (True, a)

				writtenVars = DMap.keys writes
				varChecks = map (loopCarriedDependency_varCheck loopStepTable loopIterTable loopVars (reads, writes)) writtenVars
				offendingExprs = foldl (\accum loopIT -> listConcatUnique accum (foldl (++) [] (map (loopCarriedDependency_varCheck loopStepTable loopIT loopVars_parallel (reads, writes)) writtenVars))) [] loopIterTableIterations

				inDepthFailure = (not loopIterTable_successfull) || offendingExprs /= []

				(simpleFailure, simpleOffenders) = simpleLoopCarriedDependencyCheck reads writes

				successfullExprs = (offendingExprs ++ simpleOffenders)

simpleLoopCarriedDependencyCheck :: ArrayAccessExpressions -> ArrayAccessExpressions -> (Bool, [(Expr Anno, Expr Anno)])
simpleLoopCarriedDependencyCheck reads writes = foldl (simpleLoopCarriedDependencyCheck' reads writes) (False, []) (DMap.keys writes)

simpleLoopCarriedDependencyCheck' :: ArrayAccessExpressions -> ArrayAccessExpressions -> (Bool, [(Expr Anno, Expr Anno)]) -> VarName Anno -> (Bool, [(Expr Anno, Expr Anno)])
simpleLoopCarriedDependencyCheck' reads writes (prevBool, prevPairs) var = (check_bool || prevBool, prevPairs ++ offendingPairs)
			where 
				writtenIdices = listRemoveDuplications (map (map applyGeneratedSrcSpans) (DMap.findWithDefault [] var writes))
				readIndices = listRemoveDuplications (map (map applyGeneratedSrcSpans) (DMap.findWithDefault [] var reads))
				allIndices = writtenIdices ++ readIndices

				(check_bool, (offendingReads, offendingWrites)) = foldl (simpleLoopCarriedDependencyCheck'' readIndices) (prevBool, ([],[])) writtenIdices
				offendingPairs = map (\(read, write) -> (generateArrayVar var read, generateArrayVar var write)) (zip offendingReads offendingWrites)

simpleLoopCarriedDependencyCheck'' :: [[Expr Anno]] -> (Bool, ([[Expr Anno]], [[Expr Anno]])) -> [Expr Anno] -> (Bool, ([[Expr Anno]], [[Expr Anno]]))
simpleLoopCarriedDependencyCheck'' readIndiceList (prevBool, prevPairs) writtenIndices = foldl (\(accumBool, (accumReads, accumWrites)) readIndices -> if writtenIndices == readIndices 
																								then (accumBool, (accumReads, accumWrites)) 
																								else (True, (accumReads++[readIndices],accumWrites++[writtenIndices]) ))
																							(prevBool, prevPairs) 
																							readIndiceList

loopCarriedDependency_iterative_prepareIterTable :: [VarName Anno] -> TupleTable -> [TupleTable]
loopCarriedDependency_iterative_prepareIterTable [] iterTable = [iterTable]
loopCarriedDependency_iterative_prepareIterTable loopVars (LoopIterRecord iterTable) = selections
			where
				currentLoopVar = head loopVars
				allowedValues = DMap.keys iterTable
				selections = foldl (\accum item -> accum ++ (loopCarriedDependency_iterative_prepareIterTable (tail loopVars) (DMap.findWithDefault Empty item iterTable))) [] allowedValues
				
loopCarriedDependency_varCheck :: LoopStepTable -> TupleTable -> [VarName Anno] -> (ArrayAccessExpressions, ArrayAccessExpressions) -> VarName Anno -> [(Expr Anno, Expr Anno)]
loopCarriedDependency_varCheck loopStepTable loopIterTable loopVars (reads, writes) var = offendingExprs
			where
				writtenAccesses = DMap.findWithDefault [] var writes
				readsAccesses = DMap.findWithDefault [] var reads

				offendingIndexPairs = foldl (loopCarriedDependency_writtenExprCheck loopStepTable loopIterTable loopVars readsAccesses) [] writtenAccesses
				offendingExprs = map (\(read, written) -> (generateArrayVar var read, generateArrayVar var written)) offendingIndexPairs


loopCarriedDependency_writtenExprCheck :: LoopStepTable -> TupleTable -> [VarName Anno] -> [[Expr Anno]] -> [([Expr Anno], [Expr Anno])] -> [Expr Anno] -> [([Expr Anno], [Expr Anno])]
loopCarriedDependency_writtenExprCheck loopStepTable loopIterTable loopVars readExprs oldOffendingExprs writtenExpr = oldOffendingExprs ++ dependencyPairs
			where
				offendingReads = foldl (loopCarriedDependency_readExprCheck loopStepTable loopIterTable loopVars writtenExpr) [] readExprs
				dependencyPairs = map (\x -> (x, writtenExpr)) offendingReads

loopCarriedDependency_readExprCheck :: LoopStepTable -> TupleTable -> [VarName Anno] -> [Expr Anno] -> [[Expr Anno]] -> [Expr Anno] -> [[Expr Anno]]
loopCarriedDependency_readExprCheck loopStepTable loopIterTable loopVars writtenIndexExprs oldOffendingExprs readIndexExprs 
																															| 	writtenIndices_linear && readIndices_linear && linear_noDep = oldOffendingExprs
																															| 	writtenIndices_linear && readIndices_linear && linear_depFound = oldOffendingExprs ++ [readIndexExprs]
																															| 	exhaustive_offendBool = oldOffendingExprs ++ [readIndexExprs]
																															|	otherwise = oldOffendingExprs
																												-- = if ((writtenIndices_linear) || (readIndices_linear)) && length loopVars > 2 -- (not writtenIndices_linear) || (not readIndices_linear)
																												-- then	
																												-- 	error ("writtenIndexExprs:\n" ++ (formattedWrittenIndices)
																												-- 	++ "\n\nreadIndexExprs:\n" ++ (formattedReadIndices)
																												-- 	++ "\n\nloopIterTable:\n" ++ (show loopIterTable)
																												-- 	++ "\n\nloopIterTable_optimised:\n" ++ (show loopIterTable_optimised)
																												-- 	++ "\n\nwrittenIndices_linear: " ++ (show writtenIndices_linear)
																												-- 	++ "\n\nreadIndices_linear: " ++ (show readIndices_linear)
																												-- 	++ "\n\nlinear_noDep: " ++ (show linear_noDep)
																												-- 	++ "\n\nlinear_depFound: " ++ (show linear_depFound)
																												-- 	-- ++ "\n\ncollapseIterTable_test:\n" ++ (show collapseIterTable_test)
																												-- 	-- ++ "\n\nvalueTable_optimised:\n" ++ (show valueTable_optimised)
																												-- 	-- ++ "\n\nloopVars_optimised:\n" ++ (show loopVars_optimised)
																												-- 	)
																												-- else 
																												--  	result
			where
				writtenIndices_linear = foldl (\accum item -> accum && (everything (&&) (mkQ True linearExprCheck) item)) True writtenIndexExprs
				readIndices_linear = foldl (\accum item -> accum && (everything (&&) (mkQ True linearExprCheck) item)) True readIndexExprs 

				formattedWrittenIndices = foldl (\accum item -> accum ++", " ++ outputExprFormatting item) (outputExprFormatting $ head writtenIndexExprs) (tail writtenIndexExprs)
				formattedReadIndices = foldl (\accum item -> accum ++", " ++ outputExprFormatting item) (outputExprFormatting $ head readIndexExprs) (tail readIndexExprs)
				
				(linear_noDep, linear_depFound, _, _) = loopCarriedDependency_linearCheckEvaluate [loopIterTable] loopVars readIndexExprs writtenIndexExprs (Empty, Empty) [DMap.empty]

				loopIterTable_optimised = optimiseLoopIterTable loopIterTable DMap.empty loopVars readIndexExprs writtenIndexExprs
				(exhaustive_offendBool, _, _) = loopCarriedDependency_exhaustiveEvaluate loopIterTable_optimised loopVars readIndexExprs writtenIndexExprs (False, Empty, Empty) DMap.empty

linearExprCheck :: Expr Anno -> Bool
linearExprCheck (Bin _ _ (Plus _) expr1 expr2) = True
linearExprCheck (Bin _ _ (Minus _) expr1 expr2) = True
linearExprCheck (Bin _ _ _ expr1 expr2) = False
linearExprCheck _ = True

extractLinearOffsets :: Expr Anno -> [VarName Anno] -> [(VarName Anno, Maybe(Float))]
extractLinearOffsets expr vars = offsets
			where
				operandVarNames = foldl (\accum item -> accum ++ extractVarNames item) [] (extractOperands expr)
				usedVarNames = filter (\x -> elem x operandVarNames) vars
				
				offsets = map (extractLinearOffset_var expr) usedVarNames

extractLinearOffset_var :: Expr Anno -> VarName Anno -> (VarName Anno, Maybe(Float))
extractLinearOffset_var expr var = (var, evaluated)
			where
				valueTable = addToValueTable var 0 DMap.empty
				evaluated = evaluateExpr valueTable expr



optimiseLoopIterTable :: TupleTable -> ValueTable -> [VarName Anno] -> [Expr Anno] -> [Expr Anno] -> TupleTable
optimiseLoopIterTable Empty valueTable loopVars readIndexExprs writtenIndexExprs = Empty
optimiseLoopIterTable (LoopIterRecord iterTable) valueTable loopVars readIndexExprs writtenIndexExprs = if loopVars == []  then error "optimiseLoopIterTable" else newLoopIterTable
			where
				chosenVar = head loopVars

				allowedValues = DMap.keys iterTable
				accessIterTable = (\x -> DMap.findWithDefault Empty x iterTable)

				read_chosenVarMask = maskOnVarNameUsage chosenVar readIndexExprs
				written_chosenVarMask = maskOnVarNameUsage chosenVar writtenIndexExprs

				varAffectsOutcome = read_chosenVarMask /= written_chosenVarMask

				iterTable_recurseList = map (\item -> optimiseLoopIterTable (accessIterTable item) valueTable (tail loopVars) readIndexExprs writtenIndexExprs) allowedValues
				iterTable_recurse = foldl (\accum (value, newTable) -> DMap.insert value newTable accum) iterTable (zip allowedValues iterTable_recurseList)

				newLoopIterTable = if varAffectsOutcome then LoopIterRecord iterTable_recurse else collapseIterTable (LoopIterRecord iterTable_recurse)

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
loopCarriedDependency_exhaustiveEvaluate :: TupleTable -> [VarName Anno] -> [Expr Anno] -> [Expr Anno] -> (Bool, TupleTable, TupleTable) -> ValueTable -> (Bool, TupleTable, TupleTable)
loopCarriedDependency_exhaustiveEvaluate Empty loopVars readIndexExprs writtenIndexExprs (prevCheck, prevReads, prevWrites) valueTable = 	
																																				-- if eq_exprs 
																																				-- 	then (False, newReads, newWrites)
																																				-- 	else (prevCheck || depExistsBool, newReads, newWrites)
																																				-- if 	 
																																				-- 	depExistsBool 
																																				-- 	&& vt_elems > 1
																																				-- 	then error (
																																				-- 			"\ndepExistsBool: " ++ (show depExistsBool)
																																				-- 			++ "\nreadPreviouslyWritten: " ++ (show readPreviouslyWritten)
																																				-- 			++ "\nwritePreviouslyRead: " ++ (show writePreviouslyRead)
																																				-- 			++ "\n(not readsEvaluated): " ++ (show (not readsEvaluated))
																																				-- 			++ "\n(not writesEvaluated): " ++ (show (not writesEvaluated))
 																																			-- 				++ "\nreads_eval: " ++ (show reads_eval)
 																																			-- 				++ "\nwrites_eval: " ++ (show writes_eval)
 																																			-- 				++ "\nvt_elems: " ++ (show vt_elems)
  																																		-- 					)
 																																			-- 		else 
 																																						(prevCheck || depExistsBool, newReads, newWrites)
			where
				identcalExprs = map (applyGeneratedSrcSpans) readIndexExprs == map (applyGeneratedSrcSpans) writtenIndexExprs 
				vt_elems = length (DMap.keys valueTable)

				reads_eval = map (evaluateExpr valueTable) readIndexExprs
				writes_eval = map (evaluateExpr valueTable) writtenIndexExprs

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
				depExistsBool = (not identcalExprs) && (readPreviouslyWritten || writePreviouslyRead || (not readsEvaluated) || (not writesEvaluated))

loopCarriedDependency_exhaustiveEvaluate (LoopIterRecord iterTable) loopVars readIndexExprs writtenIndexExprs previousAnalysis valueTable = if loopVars == []  then error "loopCarriedDependency_exhaustiveEvaluate" else analysis
			where
				allowedValues = DMap.keys iterTable
				valueTableIterations = map (\x -> addToValueTable (chosenVar) (fromIntegral x :: Float) valueTable) allowedValues
				accessIterTable = (\x -> DMap.findWithDefault Empty x iterTable)

				chosenVar = head loopVars
				newLoopVars = tail loopVars

				exprs1_chosenVarMask = maskOnVarNameUsage chosenVar readIndexExprs
				exprs2_chosenVarMask = maskOnVarNameUsage chosenVar writtenIndexExprs

				analysis = foldl (\accum (table, value) -> loopCarriedDependency_exhaustiveEvaluate (accessIterTable value) newLoopVars readIndexExprs writtenIndexExprs accum table) previousAnalysis (zip valueTableIterations allowedValues) 

--	This function performs a similar operation to loopCarriedDependency_exhaustiveEvaluate, except it is attempting to prove that loop carried dependencies DO NOT exist. This
--	process only works for array index expressions that are linear functions (only made up using + or -) using only loop iterators and constants but allows for the analysis
--	to be performed in constant time with respect to the number of loop iterations.
--
--	The optimisation works on the idea that linear functions follow a very simple pattern. By tracking the indices that are written to and the indices that are read, the function
--	continues analysing until it reaches a situation where there is a crossover in the domains of the reads and writes. If there has been no detected dependency by the time the
--	domains have crossed over, there will never be any dependencies because the functions are linear. The crossover is characterised by the write index tuple with the largest values
--	being larger than the read index tuple with the largest values and vice versa (largestRead > smallestWrite AND largestWrite > smallestRead)
loopCarriedDependency_linearCheckEvaluate :: [TupleTable] -> [VarName Anno] -> [Expr Anno] -> [Expr Anno] -> (TupleTable, TupleTable) -> [ValueTable] -> (Bool, Bool, TupleTable, TupleTable)
loopCarriedDependency_linearCheckEvaluate (Empty:tts) loopVars readIndexExprs writtenIndexExprs (prevReads, prevWrites) (vt:valueTables) 	|	noDepBool || depExistsBool = (noDepBool, depExistsBool, newReads, newWrites)
																																			|	otherwise = analysis_nextIter 
			where
				identcalExprs = map (applyGeneratedSrcSpans) readIndexExprs == map (applyGeneratedSrcSpans) writtenIndexExprs 
				vt_elems = length (DMap.keys vt)

				reads_eval = map (evaluateExpr vt) readIndexExprs
				writes_eval = map (evaluateExpr vt) writtenIndexExprs

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
				depExistsBool = (not identcalExprs) && (readPreviouslyWritten || writePreviouslyRead || (not readsEvaluated) || (not writesEvaluated))

				mostRead = getMostTuple newReads
				mostWritten = getMostTuple newWrites
				leastRead = getLeastTuple newReads
				leastWritten = getLeastTuple newWrites

				crossover = (tupleTableElementGreaterThan mostRead leastWritten) && (tupleTableElementGreaterThan mostWritten leastRead)

				noDepBool = crossover && (not depExistsBool)

				analysis_nextIter = loopCarriedDependency_linearCheckEvaluate tts loopVars readIndexExprs writtenIndexExprs (newReads, newWrites) valueTables

loopCarriedDependency_linearCheckEvaluate ((LoopIterRecord iterTable):tts) loopVars readIndexExprs writtenIndexExprs previousAnalysis (vt:valueTables) 	|	(analysis_noDepBool || analysis_depBool) = (analysis_noDepBool, analysis_depBool, analysis_reads, analysis_writes)
																																						|	otherwise = analysis_nextIter 
			where
				allowedValues = DMap.keys iterTable
				valueTableIterations = map (\x -> addToValueTable (chosenVar) (fromIntegral x :: Float) vt) allowedValues
				iterTableIterations = map (accessIterTable) allowedValues
				accessIterTable = (\x -> DMap.findWithDefault Empty x iterTable)

				chosenVar = head loopVars
				newLoopVars = tail loopVars

				(analysis_noDepBool, analysis_depBool, analysis_reads, analysis_writes) = loopCarriedDependency_linearCheckEvaluate iterTableIterations newLoopVars readIndexExprs writtenIndexExprs previousAnalysis valueTableIterations -- previousAnalysis (zip valueTableIterations allowedValues)
				analysis_nextIter = loopCarriedDependency_linearCheckEvaluate tts loopVars readIndexExprs writtenIndexExprs (analysis_reads, analysis_writes) valueTables
loopCarriedDependency_linearCheckEvaluate [] loopVars readIndexExprs writtenIndexExprs previousAnalysis []	=	(True, False, fst previousAnalysis, snd previousAnalysis)

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

constructLoopIterTable :: Maybe(TupleTable) -> LoopStepTable -> [VarName Anno] -> Fortran Anno -> (Maybe(TupleTable), [VarName Anno], LoopStepTable)
constructLoopIterTable Nothing loopStepTable loopVars _ = (Nothing, loopVars, loopStepTable)
constructLoopIterTable (Just (oldTable)) loopStepTable loopVars (For _ _ var e1 e2 e3 fortran) = childrenTable
			where
				childrenTable = constructLoopIterTable newLoopIterTable newLST (loopVars ++ [var]) fortran
				newLoopIterTable = extendLoopIterTable oldTable DMap.empty loopVars e1 e2 e3
				newLST = case evaluateExpr DMap.empty e3 of 
					Just value -> DMap.insert var value loopStepTable
					Nothing -> loopStepTable

constructLoopIterTable (Just (oldTable)) loopStepTable loopVars (OpenCLMap _ _ _ _ loopBoundsList fortran) = childrenTable
			where
				(var, e1, e2, e3) = head loopBoundsList
				childrenTable = constructLoopIterTable newLoopIterTable loopStepTable (loopVars ++ [var]) fortran
				newLoopIterTable = extendLoopIterTable oldTable DMap.empty loopVars e1 e2 e3

constructLoopIterTable (Just (oldTable)) loopStepTable loopVars (OpenCLReduce _ _ _ _ loopBoundsList _ fortran) = childrenTable
			where
				(var, e1, e2, e3) = head loopBoundsList
				childrenTable = constructLoopIterTable newLoopIterTable loopStepTable (loopVars ++ [var]) fortran
				newLoopIterTable = extendLoopIterTable oldTable DMap.empty loopVars e1 e2 e3

constructLoopIterTable (Just (oldTable)) loopStepTable loopVars codeSeg = (newTable, newLoopVars, newloopStepTable)
			where
				childrenAnalysis = gmapQ (mkQ (Just(Empty), [], loopStepTable) (constructLoopIterTable (Just oldTable) loopStepTable (loopVars))) codeSeg
				nonEmptyTables = filter (\(table, _, _) -> tupleTableNotEmpty (fromMaybe Empty table)) childrenAnalysis
				(newTable, newLoopVars, newloopStepTable) = if (length nonEmptyTables) == 0 then 
																		(Just oldTable, loopVars, loopStepTable)  else head nonEmptyTables


extendLoopIterTable :: TupleTable -> ValueTable -> [VarName Anno] -> Expr Anno -> Expr Anno -> Expr Anno -> Maybe(TupleTable)
extendLoopIterTable oldTable valueTable ([]) startExpr endExpr stepExpr = case range_maybe of
																			Nothing -> Nothing
																			Just range -> Just (addRangeToIterTable oldTable range)
		where
			range_maybe = evaluateRange valueTable startExpr endExpr stepExpr
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
extendLoopIterTableWithValues_foldl valueTable loopVars startExpr endExpr stepExpr (Just (LoopIterRecord oldRecord)) chosenValue = if loopVars == [] then error "extendLoopIterTableWithValues_foldl" else case newSubTable of
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