module VarAccessAnalysis where

--	The code in this file is used to analyse which variables are read and written and where in a certain
--	program. This information is then used to determine whether or not a variable in a loop can be deemed
--	temporary and therefore governs how variables are treated. Analysis also determines values/expressions
--	for variables at different points in the program. This is used when generating reduction kernels to be 
--	able to assign an initial value to reduction variables. Finally, data from this analysis phase is used
--	to differentiate between function calls and array accesses in the input source.

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List
import LanguageFortranTools
import qualified Data.Map.Strict as DMap

--	Type used to colate data on variable accesses throughout a program.
--						All reads 	All writes
type VarAccessRecord = ([SrcSpan], 	[SrcSpan])

type LocalVarAccessAnalysis = DMap.Map (VarName Anno) VarAccessRecord

type LocalVarValueAnalysis = DMap.Map (VarName Anno) [(SrcSpan, Expr Anno)]
--																			Subroutine arguments 	Declared var names
type VarAccessAnalysis = (LocalVarAccessAnalysis,	LocalVarValueAnalysis, [VarName Anno], 	[VarName Anno])

analyseAllVarAccess_progUnit :: ProgUnit Anno -> VarAccessAnalysis
analyseAllVarAccess_progUnit progUnit = analyseAllVarAccess [progUnit]

analyseAllVarAccess :: Program Anno -> VarAccessAnalysis
analyseAllVarAccess prog = (localVarAccesses, localVarValues, arguments, declarations)
						where
							--	LocalVarAccesses is made up of information on all of the reads and writes throughout
							--	the program being analysed. It is a list of triplets where each triplet contains a 
							--	VarName, a list of read locations of that VarName and a list of write locations of that
							--	VarName
							localVarAccesses = analyseLocalVarAccess declarations prog
							--	Arguments to this program block are treated as de facto non temporary variables,
							--	due to the fact that arguments are passed by reference by default in fortran.
							arguments = getArguments prog

							--	The main motivation for the tracking the declarations at the top of a program is for
							--	differentiating function calls from array accesses as Language-Fortran does not do this
							--	automatically it seems. 
							declarations = everything (++) (mkQ [] getDeclaredVarNames) prog

							localVarValues = everything (combineMaps) (mkQ DMap.empty analyseAllVarValues_fortran) prog

analyseLocalVarAccess :: [VarName Anno] -> Program Anno -> LocalVarAccessAnalysis
analyseLocalVarAccess declarations prog = analysis
				where
					blockAnalysis = foldl (\accum item -> accum ++ (gmapQ (mkQ DMap.empty (analyseAllVarAccess_block_delta declarations)) item)) [] prog
					progUnitAnalysis = foldl (\accum item -> accum ++ (gmapQ (mkQ DMap.empty (analyseLocalVarAccess declarations)) item)) [] prog
					analysis = foldl (combineLocalVarAccessAnalysis) DMap.empty (blockAnalysis ++ progUnitAnalysis)

--	Since Language-Fortran does not seem to differentate between function calls and array access, it was necessary
--	to find a way to identify a function call. This function acheives that. When an expression is passed in, a top level
--	VarNames is extracted (The possibility for multipe varnames is also dealt with here as the Language-Fortran
--	specifcation allows for this). A check to see whether this VarName was NOT declared at the top of the program is done 
--	and a check to see whether the expr in question contains other expressions is also performed. If both of these checks
--	pass then the expr is a function call. (The second check here comes from the fact that arguments that a function is
--	called with are stored in a list inside the original expr. If there are no arguments to the function, a NullExpr
--	object can be found. For a normal scaler value, there would be absolutely nothing in this internal list, not even a 
--	NullExpr object)
isFunctionCall :: VarAccessAnalysis -> Expr Anno -> Bool
isFunctionCall accessAnalysis expr =  (all (\x -> not (elem x declaredVarNames)) exprVarNames) && subVars /= []
						where 
							subVars = extractContainedVars expr
							exprVarNames = extractVarNames expr
							declaredVarNames = (\(_,_,_,x) -> x) accessAnalysis

isFunctionCall_varNames :: [VarName Anno] -> Expr Anno -> Bool
isFunctionCall_varNames declaredVarNames expr =  (all (\x -> not (elem x declaredVarNames)) exprVarNames) && subVars /= []
						where 
							subVars = extractContainedVars expr
							exprVarNames = extractVarNames expr

isNullExpr :: Expr Anno -> Bool
isNullExpr (NullExpr _ _) = True
isNullExpr _ = False

getAccessesBetweenSrcSpansIgnore :: VarAccessAnalysis -> SrcSpan -> SrcSpan -> [SrcSpan] -> ([VarName Anno], [VarName Anno])
getAccessesBetweenSrcSpansIgnore accessAnalysis (_,startLoc) (endLoc,_) skipSrcs = getAccessesBetweenManySrcSpans accessAnalysis allowedSrcSpans -- ([], [])
		where
			sortedSkipSrcs = sortBy srcSpanCompare skipSrcs
			allowedSrcSpans = getAccessesBetweenSrcSpansIgnoreBuildSrcSpans startLoc endLoc skipSrcs

getAccessesBetweenSrcSpansIgnoreBuildSrcSpans :: SrcLoc -> SrcLoc -> [SrcSpan] -> [(SrcLoc, SrcLoc)]
getAccessesBetweenSrcSpansIgnoreBuildSrcSpans prevEndLoc finalEndLoc [] = [(prevEndLoc, finalEndLoc)]
getAccessesBetweenSrcSpansIgnoreBuildSrcSpans prevEndLoc finalEndLoc ((startLoc, endLoc):skipSrcs) = [(prevEndLoc, startLoc)] ++ getAccessesBetweenSrcSpansIgnoreBuildSrcSpans endLoc finalEndLoc skipSrcs

srcSpanCompare :: SrcSpan -> SrcSpan -> Ordering
srcSpanCompare ((SrcLoc f1 l1 c1), _) ((SrcLoc f2 l2 c2), _) 	|	l1 < l2 || (l1 == l2 && c1 < c2) 	= LT
																|	l1 > l2 || (l1 == l2 && c1 > c2) 	= GT
																|	l1 == l2 && c1 == c2 				= EQ

getAccessesBetweenManySrcSpans ::  VarAccessAnalysis -> [(SrcLoc, SrcLoc)] -> ([VarName Anno], [VarName Anno])
getAccessesBetweenManySrcSpans accessAnalysis [] = ([],[])
getAccessesBetweenManySrcSpans accessAnalysis ((startLoc, endLoc):srcs) = ((listConcatUnique currentReads followingReads), (listConcatUnique currentWrites followingWrites))
		where
			(currentReads, currentWrites) = getAccessesBetweenSrcSpans accessAnalysis startLoc endLoc
			(followingReads, followingWrites) = getAccessesBetweenManySrcSpans accessAnalysis srcs

getAccessesAfterSrcSpan :: VarAccessAnalysis -> SrcLoc -> ([VarName Anno], [VarName Anno])
getAccessesAfterSrcSpan accessAnalysis startLoc = (reads, writes)
		where
			localVarAccesses = (\(x,_,_,_) -> x) accessAnalysis
			allVars = DMap.keys localVarAccesses
			reads = filter (varReadAfterSrcLoc localVarAccesses startLoc) allVars
			writes = filter (varWrittenAfterSrcLoc localVarAccesses startLoc) allVars

varWrittenAfterSrcLoc :: LocalVarAccessAnalysis -> SrcLoc -> VarName Anno -> Bool
varWrittenAfterSrcLoc localVarAccesses loc var = appearance
		where
			writes = map (fst) (snd (DMap.findWithDefault ([],[]) var localVarAccesses) )
			appearance = foldl (\accum item -> accum || ((checkSrcLocBefore loc item) )) False writes

varReadAfterSrcLoc :: LocalVarAccessAnalysis -> SrcLoc -> VarName Anno -> Bool
varReadAfterSrcLoc localVarAccesses loc var = appearance
		where
			reads = map (fst) (fst (DMap.findWithDefault ([],[]) var localVarAccesses)) 
			appearance = foldl (\accum item -> accum || ((checkSrcLocBefore loc item) )) False reads

getAccessesBeforeSrcSpan :: VarAccessAnalysis -> SrcLoc -> ([VarName Anno], [VarName Anno])
getAccessesBeforeSrcSpan accessAnalysis endLoc = getAccessesBetweenSrcSpans accessAnalysis (SrcLoc "" 0 0) endLoc

getAccessesBetweenSrcSpans :: VarAccessAnalysis -> SrcLoc -> SrcLoc -> ([VarName Anno], [VarName Anno])
getAccessesBetweenSrcSpans accessAnalysis startLoc endLoc = (reads, writes)
		where
			localVarAccesses = (\(x,_,_,_) -> x) accessAnalysis
			allVars = DMap.keys localVarAccesses
			reads = filter (varReadInRange localVarAccesses startLoc endLoc) allVars
			writes = filter (varWrittenInRange localVarAccesses startLoc endLoc) allVars

varReadInRange :: LocalVarAccessAnalysis -> SrcLoc -> SrcLoc -> VarName Anno -> Bool
varReadInRange localVarAccesses startLoc endLoc var = appearance
		where
			reads = map (fst) (fst (DMap.findWithDefault ([],[]) var localVarAccesses)) 
			appearance = foldl (\accum item -> accum || ((checkSrcLocBefore startLoc item) && (checkSrcLocBefore item endLoc))) False reads

varWrittenInRange :: LocalVarAccessAnalysis -> SrcLoc -> SrcLoc -> VarName Anno -> Bool
varWrittenInRange localVarAccesses startLoc endLoc var = appearance
		where
			writes = map (fst) (snd (DMap.findWithDefault ([],[]) var localVarAccesses) )
			appearance = foldl (\accum item -> accum || ((checkSrcLocBefore startLoc item) && (checkSrcLocBefore item endLoc))) False writes

-- checkSrcLocBefore

getArguments :: Program Anno -> [VarName Anno]
getArguments prog = argNames
		where
			argNames = everything (++) (mkQ [] getArgNamesAsVarNames) prog 

getArguments_list :: Arg Anno -> [VarName Anno]
getArguments_list arg = everything (++) (mkQ [] getArgNamesAsVarNames) arg

getArgNamesAsVarNames :: ArgName Anno -> [VarName Anno]
getArgNamesAsVarNames (ArgName _ str) = [VarName nullAnno str]
getArgNamesAsVarNames _ = []

getDeclaredVarNames :: Decl Anno -> [VarName Anno]
getDeclaredVarNames (Decl _ _ lst _) = foldl (\accum (expr1, _, _) -> accum ++ extractVarNames expr1) [] lst
getDeclaredVarNames decl = []

analyseAllVarValues_fortran :: Fortran Anno -> LocalVarValueAnalysis
analyseAllVarValues_fortran (Assg _ src expr1 expr2) = foldl (\accum item -> appendToMap item (src, expr2) accum) DMap.empty varnames
								where
									varnames = extractVarNames expr1
analyseAllVarValues_fortran _ = DMap.empty


analyseAllVarAccess_block :: [VarName Anno] -> Block Anno -> LocalVarAccessAnalysis
analyseAllVarAccess_block declarations (Block _ _ _ _ _ fort) = everything (combineLocalVarAccessAnalysis) (mkQ DMap.empty (analyseAllVarAccess_fortran declarations)) fort

--	Function compiles the lists of read and write accesses for the code in question. The function is recursive and makes use of SYB.
--	There are two cases, either the current piece of code is an assignment to a variable or it is not. If the code is an assignment
--	then there must be additions made to the set of writes for a variable, as well as the set(s) of reads for some variable(s). In
--	the other case, only reads must be added.
analyseAllVarAccess_fortran :: [VarName Anno] -> Fortran Anno -> LocalVarAccessAnalysis
analyseAllVarAccess_fortran declarations (Assg _ _ writeExpr readExpr) = analysis'
												where
													readExprs = extractOperands readExpr
													readVarNames = foldl (collectVarNames_foldl declarations) [] readExprs
													writtenVarNames = extractVarNames writeExpr

													analysis = foldl (addVarReadAccess (srcSpan readExpr)) DMap.empty readVarNames
													analysis' = foldl (addVarWriteAccess (srcSpan writeExpr)) DMap.empty writtenVarNames
analyseAllVarAccess_fortran declarations (If _ _ readExpr _ _ _) = analysis
												where
													readExprs = extractOperands readExpr
													readVarNames = foldl (collectVarNames_foldl declarations) [] readExprs

													analysis = foldl (addVarReadAccess (srcSpan readExpr)) DMap.empty readVarNames
analyseAllVarAccess_fortran declarations codeSeg = analysis
												where 
													extractedExprs = gmapQ (mkQ (Null nullAnno nullSrcSpan) extractExpr) codeSeg
													extractedOperands = foldl (\accum item -> accum ++ extractOperands item) [] extractedExprs
													readVarNames = foldl (collectVarNames_foldl declarations) [] extractedOperands

													analysis = foldl (addVarReadAccess (srcSpan codeSeg)) DMap.empty readVarNames	

analyseAllVarAccess_block_delta :: [VarName Anno] -> Block Anno -> LocalVarAccessAnalysis
analyseAllVarAccess_block_delta declarations (Block _ _ _ _ _ fortran) = analyseAllVarAccess_fortran_delta declarations DMap.empty fortran

analyseAllVarAccess_fortran_delta :: [VarName Anno] -> LocalVarAccessAnalysis -> Fortran Anno -> LocalVarAccessAnalysis
analyseAllVarAccess_fortran_delta declarations prevAnalysis codeSeg = case codeSeg of
									Assg _ _ writeExpr readExpr -> analysis'
												where
													readExprs = extractOperands readExpr
													readVarNames = foldl (collectVarNames_foldl declarations) [] readExprs
													writtenVarNames = extractVarNames writeExpr

													analysis = foldl (addVarReadAccess (srcSpan readExpr)) prevAnalysis readVarNames
													analysis' = foldl (addVarWriteAccess (srcSpan writeExpr)) analysis writtenVarNames
									If _ _ readExpr _ _ _ -> analysis

												where
													readExprs = extractOperands readExpr
													readVarNames = foldl (collectVarNames_foldl declarations) [] readExprs

													analysis = foldl (addVarReadAccess (srcSpan readExpr)) prevAnalysis readVarNames
									_ -> analysisIncChildren 
												where 
													extractedExprs = gmapQ (mkQ (Null nullAnno nullSrcSpan) extractExpr) codeSeg
													extractedOperands = foldl (\accum item -> accum ++ extractOperands item) [] extractedExprs
													readVarNames = foldl (collectVarNames_foldl declarations) [] extractedOperands

													analysis = foldl (addVarReadAccess (srcSpan codeSeg)) prevAnalysis readVarNames

													analysisIncChildren_list = gmapQ (mkQ (DMap.empty) (analyseAllVarAccess_fortran_delta declarations analysis)) codeSeg
													analysisIncChildren = foldl (combineLocalVarAccessAnalysis) DMap.empty analysisIncChildren_list


collectVarNames :: [VarName Anno] -> Expr Anno -> [VarName Anno]
collectVarNames declarations item = varnames
						where
							fnCall = isFunctionCall_varNames declarations item
							fnArgs = extractContainedVars item
							varnames = case fnCall of
								True -> foldl (\accum item -> accum ++ extractVarNames item) [] fnArgs
								False -> extractVarNames item

collectVarNames_foldl :: [VarName Anno] -> [VarName Anno] -> Expr Anno -> [VarName Anno]
collectVarNames_foldl declarations accum item = accum ++ collectVarNames declarations item

getValueAtSrcSpan :: VarName Anno -> SrcSpan -> VarAccessAnalysis -> Expr Anno
getValueAtSrcSpan varname target_src (_, analysis, _, _) = valueAtSrc
								where
									values = DMap.findWithDefault [] varname analysis
									valueAtSrc = foldl (\accum (item_src, expr) -> if checkSrcSpanBefore item_src target_src then expr else accum) (NullExpr nullAnno nullSrcSpan) values

getAccessedExprs :: [VarName Anno] -> [Expr Anno] -> Expr Anno -> [Expr Anno]
getAccessedExprs declarations accum item = case fnCall of
											True ->	accum ++ extractContainedVars item
											False -> accum ++ extractOperands item
										where 
											fnCall = isFunctionCall_varNames declarations item

-- 	Recursive function to add a record of a read for a certain VarName
addVarReadAccess :: SrcSpan -> LocalVarAccessAnalysis -> VarName Anno -> LocalVarAccessAnalysis
addVarReadAccess srcspan analysis varname = DMap.insert varname (newAccessRecord) analysis
										where
											(oldReads, oldWrites) = (DMap.findWithDefault ([],[]) varname analysis)
											newAccessRecord = (oldReads ++ [srcspan], oldWrites)

-- 	Recursive function to add a record of a write for a certain VarName
addVarWriteAccess :: SrcSpan -> LocalVarAccessAnalysis -> VarName Anno -> LocalVarAccessAnalysis
addVarWriteAccess srcspan analysis varname = DMap.insert varname (newAccessRecord) analysis
										where
											(oldReads, oldWrites) = (DMap.findWithDefault ([],[]) varname analysis)
											newAccessRecord = (oldReads, oldWrites ++ [srcspan])														

combineVarAccessAnalysis :: VarAccessAnalysis -> VarAccessAnalysis -> VarAccessAnalysis
combineVarAccessAnalysis analysis1 analysis2 = resultantAnalysis
						where
							(varAccess1, varValue1, subArgs1, declared1) = analysis1
							(varAccess2, varValue2, subArgs2, declared2) = analysis2
							varAccessComb = combineLocalVarAccessAnalysis varAccess1 varAccess2
							varValueComb = combineMaps varValue1 varValue2
							subArgsComb = subArgs1 ++ subArgs2
							declaredComb = declared1 ++ declared2

							resultantAnalysis = (varAccessComb, varValueComb, subArgsComb, declaredComb)


--	Helper function used to bring together sets of variable access analysis records.
combineLocalVarAccessAnalysis :: LocalVarAccessAnalysis -> LocalVarAccessAnalysis -> LocalVarAccessAnalysis
combineLocalVarAccessAnalysis analysis1 analysis2 = resultantAnalysis
						where
							analysis2List = DMap.toList analysis2
							resultantAnalysis = foldl (\accum (key, value) -> DMap.insert key (combineBinaryListTuples (DMap.findWithDefault ([],[]) key accum) value) accum) analysis1 analysis2List

combineBinaryListTuples :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
combineBinaryListTuples (a1, b1) (a2, b2) = (a1 ++ a2, b1 ++ b2)

--	The function is directly called by Transformer.hs when it is attempting to parallelise a certain loop. This function is supplied with a start
--	and end point for a loop (SrcSpan) and the VarAccessAnalysis record for the program. The returned list is all of the VarNames that must be
--	considdered non temporary for that loop. For a variable to be considered non temporary, it must either be an argument to this code block or
--	it must be read after the end of the loop, before any data is written to it. In the second case, this means that a variable is non temporary
--	if the final value left in it by the loop is read and used elsewhere.
getNonTempVars :: SrcSpan -> VarAccessAnalysis -> [VarName Anno]
getNonTempVars codeBlockSpan accessAnalysis = hangingReads ++ subroutineArguments
						where
							localVarAccesses = (\(x,_, _, _) -> x) accessAnalysis
							subroutineArguments = (\(_,_, x, _) -> x) accessAnalysis
							readsAfterBlock = varAccessAnalysis_readsAfter codeBlockSpan localVarAccesses
							writesReadsAfterBlock = varAccessAnalysis_writesAfter codeBlockSpan readsAfterBlock
							hangingReads = filter (checkHangingReads writesReadsAfterBlock) (DMap.keys writesReadsAfterBlock)							

varAccessAnalysis_writesAfter :: SrcSpan -> LocalVarAccessAnalysis -> LocalVarAccessAnalysis
varAccessAnalysis_writesAfter codeBlockSpan accessAnalysis = foldl (varAccessAnalysis_writesAfter' codeBlockSpan accessAnalysis) DMap.empty (DMap.keys accessAnalysis)


varAccessAnalysis_writesAfter' :: SrcSpan -> LocalVarAccessAnalysis -> LocalVarAccessAnalysis -> VarName Anno -> LocalVarAccessAnalysis
varAccessAnalysis_writesAfter' (_, SrcLoc _ line_end _) accessAnalysis accumAnalysis varname = combineLocalVarAccessAnalysis accumAnalysis outputAnalysis
						where
							(readSpans, writeSpans) = DMap.findWithDefault ([], []) varname accessAnalysis
							newWriteSpans = filter (\((SrcLoc _ line_write column_write), _) -> line_write >= line_end) writeSpans
							outputAnalysis = DMap.insert varname (readSpans, newWriteSpans) DMap.empty

varAccessAnalysis_readsAfter :: SrcSpan -> LocalVarAccessAnalysis -> LocalVarAccessAnalysis
varAccessAnalysis_readsAfter codeBlockSpan accessAnalysis = foldl (varAccessAnalysis_readsAfter' codeBlockSpan accessAnalysis) DMap.empty (DMap.keys accessAnalysis)


varAccessAnalysis_readsAfter' :: SrcSpan -> LocalVarAccessAnalysis -> LocalVarAccessAnalysis -> VarName Anno -> LocalVarAccessAnalysis
varAccessAnalysis_readsAfter' (_, SrcLoc _ line_end _) accessAnalysis accumAnalysis varname = combineLocalVarAccessAnalysis accumAnalysis outputAnalysis
						where
							(readSpans, writeSpans) = DMap.findWithDefault ([], []) varname accessAnalysis
							newReadSpans = filter (\((SrcLoc _ line_read column_read), _) -> line_read >= line_end) readSpans
							outputAnalysis = DMap.insert varname (newReadSpans, writeSpans) DMap.empty

checkHangingReads :: LocalVarAccessAnalysis -> VarName Anno -> Bool
checkHangingReads analysis varname = case earliestRead of
														Just r ->	case earliestWrite of
																		Just w -> not (checkSrcSpanBefore_line w r)
																		Nothing -> True
														Nothing ->	False
								where 
									(readSpans, writeSpans) = DMap.findWithDefault ([], []) varname analysis
									earliestRead = getEarliestSrcSpan readSpans
									earliestWrite = getEarliestSrcSpan writeSpans

getEarliestSrcSpan :: [SrcSpan] -> Maybe(SrcSpan)
getEarliestSrcSpan [] = Nothing
getEarliestSrcSpan spans = Just (foldl (\accum item -> if checkSrcSpanBefore item accum then item else accum) (spans!!0) spans)

getLatestSrcSpan :: [SrcSpan] -> Maybe(SrcSpan)
getLatestSrcSpan [] = Nothing
getLatestSrcSpan spans = Just (foldl (\accum item -> if checkSrcSpanBefore item accum then accum else item) (spans!!0) spans)

checkSrcLocBefore :: SrcLoc -> SrcLoc -> Bool
checkSrcLocBefore (SrcLoc file_before line_before column_before) (SrcLoc file_after line_after column_after) =  (line_before < line_after) || ((line_before == line_after) && (column_before < column_after))

checkSrcSpanBefore :: SrcSpan -> SrcSpan -> Bool
-- checkSrcSpanBefore (beforeStart, (SrcLoc file_before line_before column_before)) ((SrcLoc file_after line_after column_after), afterEnd) = (line_before < line_after) || ((line_before == line_after) && (column_before < column_after))
checkSrcSpanBefore ((SrcLoc file_before line_before column_before), beforeEnd) ((SrcLoc file_after line_after column_after), afterEnd) = (line_before < line_after) || ((line_before == line_after) && (column_before < column_after))

checkSrcSpanBefore_line :: SrcSpan -> SrcSpan -> Bool
checkSrcSpanBefore_line ((SrcLoc file_before line_before column_before), beforeEnd) ((SrcLoc file_after line_after column_after), afterEnd) = (line_before < line_after)
