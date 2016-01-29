module VarAccessAnalysis where

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List
import LanguageFortranTools

--	Type used to colate data on variable accesses throughout a program.
--						Name of variable 	All reads 	All writes
type VarAccessRecord = (VarName [String], 	[SrcSpan], 	[SrcSpan])
type LocalVarAccessAnalysis = [VarAccessRecord]
type VarAccessAnalysis = (LocalVarAccessAnalysis, [VarName [String]])

--analyseAllVarAccess:: Block [String] -> VarAccessAnalysis
--analyseAllVarAccess block = (localVarAccesses, arguments)
--						where 
--							localVarAccesses = foldl (combineVarAccessAnalysis) [] (gmapQ (mkQ [] (analyseAllVarAccess_fortran [])) block)
--							arguments = getArguments

analyseAllVarAccess:: Program [String] -> VarAccessAnalysis
analyseAllVarAccess prog = (localVarAccesses, arguments)
						where 
							--localVarAccesses = foldl (combineVarAccessAnalysis) [] (gmapQ (mkQ [] (analyseAllVarAccess_fortran [])) block)
							localVarAccesses = everything (combineVarAccessAnalysis) (mkQ [] (analyseAllVarAccess_fortran [])) prog
							arguments = getArguments prog

getNonTempVars :: SrcSpan -> VarAccessAnalysis -> [VarName [String]]
getNonTempVars codeBlockSpan accessAnalysis = (map (\(x, _, _) -> x) hangingReads) ++ subroutineArguments
						where
							localVarAccesses = fst accessAnalysis
							subroutineArguments = snd accessAnalysis
							readsAfterBlock = varAccessAnalysis_readsAfter codeBlockSpan localVarAccesses
							writesReadsAfterBlock = varAccessAnalysis_writesAfter codeBlockSpan readsAfterBlock
							hangingReads = filter (checkHangingReads) writesReadsAfterBlock

getArguments :: Program [String] -> [VarName [String]]
getArguments prog = argNames
		where
			argNames = everything (++) (mkQ [] getArgNamesAsVarNames) prog--foldl (++) [] (foldl (++) [] (map (gmapQ (mkQ [] getArguments_list)) prog))

getArguments_list :: Arg [String] -> [VarName [String]]
getArguments_list arg = everything (++) (mkQ [] getArgNamesAsVarNames) arg

getArgNamesAsVarNames :: ArgName [String] -> [VarName [String]]
getArgNamesAsVarNames (ArgName _ str) = [VarName [] str]
getArgNamesAsVarNames _ = []


analyseAllVarAccess_fortran :: LocalVarAccessAnalysis -> Fortran [String] -> LocalVarAccessAnalysis
analyseAllVarAccess_fortran prevAnalysis (Assg _ _ writeExpr readExpr) = combineVarAccessAnalysis prevAnalysis analysisWithWritesReads
												where
													readVarNames = foldl (\accum item -> accum ++ (extractVarNames item)) [] (extractOperands readExpr)
													writtenVarNames = foldl (\accum item -> accum ++ (extractVarNames item)) [] (extractOperands writeExpr)
													analysisWithReads = foldl (addVarReadAccess (srcSpan readExpr)) prevAnalysis readVarNames
													analysisWithWritesReads = foldl (addVarWriteAccess (srcSpan readExpr)) analysisWithReads writtenVarNames

analyseAllVarAccess_fortran prevAnalysis codeSeg =  foldl (combineVarAccessAnalysis) [] (gmapQ (mkQ [] (analyseAllVarAccess_fortran currentAnalysis)) codeSeg)
												where 
													extractedExprs = gmapQ (mkQ (Null [] generatedSrcSpan) extractExprs) codeSeg
													extractedOperands = foldl (\accum item -> accum ++ extractOperands item) [] extractedExprs
													readVarNames = foldl (\accum item -> accum ++ extractVarNames item) [] extractedOperands	
													analysisWithReads = foldl (addVarReadAccess (srcSpan codeSeg)) prevAnalysis readVarNames	
													currentAnalysis = combineVarAccessAnalysis prevAnalysis analysisWithReads							

addVarReadAccess :: SrcSpan -> LocalVarAccessAnalysis -> VarName [String] -> LocalVarAccessAnalysis
addVarReadAccess srcspan ((varnameAnalysis, src_reads, src_writes):xs) varname  | varnameAnalysis == varname = [(varname, src_reads ++ [srcspan], src_writes)] ++ xs
																				| otherwise = [(varnameAnalysis, src_reads, src_writes)] ++ (addVarReadAccess srcspan xs varname)
addVarReadAccess srcspan [] varname	= [(varname, [srcspan], [])]

addVarWriteAccess :: SrcSpan -> LocalVarAccessAnalysis -> VarName [String] -> LocalVarAccessAnalysis
addVarWriteAccess srcspan ((varnameAnalysis, src_reads, src_writes):xs) varname  | varnameAnalysis == varname = [(varname, src_reads, src_writes ++ [srcspan])] ++ xs
																				| otherwise = [(varnameAnalysis, src_reads, src_writes)] ++ (addVarWriteAccess srcspan xs varname)
addVarWriteAccess srcspan [] varname	= [(varname, [], [srcspan])]															

combineVarAccessAnalysis :: LocalVarAccessAnalysis -> LocalVarAccessAnalysis -> LocalVarAccessAnalysis
combineVarAccessAnalysis a b = foldl (addVarAccessAnalysis) a b 

addVarAccessAnalysis :: LocalVarAccessAnalysis -> (VarName [String], [SrcSpan], 	[SrcSpan]) -> LocalVarAccessAnalysis
addVarAccessAnalysis ((varnameAnalysis, readsAnalysis, writesAnalysis):xs) (newVarName, newReads, newWrites) 	| varnameAnalysis == newVarName = [(varnameAnalysis, readsAnalysis ++ newReads, writesAnalysis ++ newWrites)] ++ xs
																												| otherwise = [(varnameAnalysis, readsAnalysis, writesAnalysis)] ++ (addVarAccessAnalysis xs (newVarName, newReads, newWrites))
addVarAccessAnalysis [] (newVarName, newReads, newWrites) = [(newVarName, newReads, newWrites)]

varAccessAnalysis_writesAfter :: SrcSpan -> LocalVarAccessAnalysis -> LocalVarAccessAnalysis
varAccessAnalysis_writesAfter codeBlockSpan accessAnalysis = foldl (\accum item -> accum ++ varAccessAnalysis_writesAfter' codeBlockSpan item) [] accessAnalysis

varAccessAnalysis_writesAfter' :: SrcSpan -> VarAccessRecord ->  LocalVarAccessAnalysis
varAccessAnalysis_writesAfter' (start, SrcLoc file_end line_end column_end) (varname, readSpans, writeSpans) = [(varname, readSpans, newWriteSpans)]
										where
											newWriteSpans = filter (\((SrcLoc _ line_write column_write), _) -> line_write >= line_end) writeSpans

varAccessAnalysis_readsAfter :: SrcSpan -> LocalVarAccessAnalysis -> LocalVarAccessAnalysis
varAccessAnalysis_readsAfter codeBlockSpan accessAnalysis = foldl (\accum item -> accum ++ varAccessAnalysis_readsAfter' codeBlockSpan item) [] accessAnalysis

varAccessAnalysis_readsAfter' :: SrcSpan -> VarAccessRecord ->  LocalVarAccessAnalysis
varAccessAnalysis_readsAfter' (start, SrcLoc file_end line_end column_end) (varname, readSpans, writeSpans) = if newReadSpans /= [] then [(varname, newReadSpans, writeSpans)] else []
										where
											newReadSpans = filter (\((SrcLoc _ line_read column_read), _) -> line_read >= line_end) readSpans

checkHangingReads :: VarAccessRecord -> Bool
checkHangingReads (varname, readSpans, writeSpans) = case earliestRead of
														Just r ->	case earliestWrite of
																		Just w -> checkSrcSpanBefore r w
																		Nothing -> True
														Nothing ->	False
								where 
									earliestRead = getEarliestSrcSpan readSpans
									earliestWrite = getEarliestSrcSpan writeSpans

getEarliestSrcSpan :: [SrcSpan] -> Maybe(SrcSpan)
getEarliestSrcSpan [] = Nothing
getEarliestSrcSpan spans = Just (foldl (\accum item -> if checkSrcSpanBefore item accum then item else accum) (spans!!0) spans)

checkSrcSpanBefore :: SrcSpan -> SrcSpan -> Bool
checkSrcSpanBefore ((SrcLoc file_before line_before column_before), beforeEnd) ((SrcLoc file_after line_after column_after), afterEnd) = (line_before < line_after) && (column_before < column_after)