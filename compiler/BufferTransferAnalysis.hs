module BufferTransferAnalysis where

import LanguageFortranTools
import VarAccessAnalysis
import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Data.Maybe
import Language.Fortran
import LanguageFortranTools
import qualified Data.Map as DMap

type SubroutineTable = DMap.Map String (ProgUnit Anno, String)

replaceSubroutineAppearences :: SubroutineTable -> [Program Anno] -> [Program Anno]
replaceSubroutineAppearences subTable [] = []
replaceSubroutineAppearences subTable (firstProgram:programs) = updated:replaceSubroutineAppearences subTable programs
		where 
			updated = everywhere (mkT (replaceSubroutine subTable)) firstProgram

-- flattenSubroutineAppearences :: SubroutineTable -> Program Anno -> Program Anno
flattenSubroutineAppearences subTable mainAst = updated
		where
			subroutines = DMap.keys subTable
			updated = everywhere (mkT (flattenSubroutineCall_container subTable)) mainAst
			-- updated = everywhere (mkT (flattenSubroutineCall subTable)) mainAst

flattenSubroutineCall_container :: SubroutineTable -> Fortran Anno -> Fortran Anno
flattenSubroutineCall_container subTable containerSeg 	| 	containedCalls /= [] = containerWithFlattenedSub
													|	otherwise = containerSeg
		where
			containedCalls = foldl (++) [] (gmapQ (mkQ [] extractCalls) containerSeg)
			(Call _ _ callExpr _) = if (length containedCalls > 1) then error "flattenSubroutineCall_container: multiple contained calls unsupported" else head containedCalls

			subroutineSrc = case DMap.lookup subroutineName subTable of
										Nothing -> nullSrcSpan
										Just (subroutineBody, _) -> srcSpan subroutineBody
			subroutineLineSize = srcSpanLineCount subroutineSrc
			containerWithScrShifts = gmapT (mkT (ignoreCall_T (shiftSrcSpanLineGlobal subroutineLineSize))) containerSeg

			subroutineName = if extractVarNames callExpr == [] then (error "flattenSubroutineCall:callExpr\n" ++ (show callExpr))  else varNameStr (head (extractVarNames callExpr))
			containerWithFlattenedSub = gmapT (mkT (flattenSubroutineCall subTable)) containerWithScrShifts

flattenSubroutineCall :: SubroutineTable -> Fortran Anno -> Fortran Anno
flattenSubroutineCall subTable (Call anno cSrc callExpr args) = fromMaybe callFortran shiftedSubroutineReplacement
		where
			callFortran = (Call anno cSrc callExpr args)
			subroutineName = if extractVarNames callExpr == [] then (error "flattenSubroutineCall:callExpr\n" ++ (show callExpr))  else varNameStr (head (extractVarNames callExpr))
			subroutineReplacement = case DMap.lookup subroutineName subTable of
										Nothing -> Nothing
										Just (subroutineBody, _) -> Just (substituteArguments callFortran subroutineBody)

			subroutineSrc = srcSpan (fromMaybe (error "flattenSubroutineCall: subroutineSrc") subroutineReplacement)
			((SrcLoc _ callLineStart _), _) =  cSrc
			((SrcLoc _ bodyLineStart _), _) =  subroutineSrc
			bodyOffset = (callLineStart -  bodyLineStart) + 1

			shiftedSubroutineReplacement = case subroutineReplacement of
												Nothing -> Nothing
												Just rep -> Just (shiftSrcSpanLineGlobal bodyOffset rep) 
flattenSubroutineCall subTable codeSeg = codeSeg

ignoreCall_T :: (Fortran Anno -> Fortran Anno) -> Fortran Anno -> Fortran Anno
ignoreCall_T func codeSeg = case codeSeg of
								Call _ _ _ _ -> codeSeg
								_ -> func codeSeg

extractCalls :: Fortran Anno -> [Fortran Anno]
extractCalls codeSeg = case codeSeg of
							Call _ _ _ _ -> [codeSeg]
							_ -> []

extractCallsWithStrings :: Fortran Anno -> [(Fortran Anno, String)]
extractCallsWithStrings codeSeg = case codeSeg of
							Call _ _ callExpr _ -> [(codeSeg, varNameStr $ head (extractVarNames callExpr))]
							_ -> []

-- flattenSubroutineCall :: SubroutineTable -> Fortran Anno -> Fortran Anno
-- flattenSubroutineCall subTable (FSeq anno src callFortran otherFortran) 	
-- 										|	isCall = case subroutineReplacement of
-- 																		Nothing -> (FSeq anno src callFortran otherFortran)
-- 																		Just flattened -> -- error ("bodyOffset: " ++ (show bodyOffset) ++ "\nsrc: " ++ (show src))
-- 																						-- stretchSrcSpanLine subroutineLineSize 
-- 																						(FSeq anno nullSrcSpan 
-- 																							(shiftSrcSpanLineGlobal bodyOffset flattened) 
-- 																							(shiftSrcSpanLineGlobal subroutineLineSize otherFortran))
-- 										|	otherwise = (FSeq anno src callFortran otherFortran)
-- 		where 
-- 			(isCall, cSrc, callExpr) = case callFortran of
-- 							(Call _ cSrc expr _) -> (True, cSrc, expr)
-- 							_ -> (False, error "flattenSubroutineCall", error "flattenSubroutineCall")
-- 			((SrcLoc _ callLineStart _), _) =  cSrc
-- 			((SrcLoc _ bodyLineStart _), _) =  subroutineSrc
-- 			bodyOffset = (callLineStart -  bodyLineStart) + 1

-- 			subroutineLineSize = srcSpanLineCount subroutineSrc
-- 			subroutineName = if extractVarNames callExpr == [] then (error "flattenSubroutineCall:callExpr\n" ++ (show callExpr))  else varNameStr (head (extractVarNames callExpr))
-- 			subroutineReplacement = case DMap.lookup subroutineName subTable of
-- 										Nothing -> Nothing
-- 										Just (subroutineBody, _) -> Just (substituteArguments callFortran subroutineBody)
-- 			subroutineSrc = srcSpan (fromMaybe (error "flattenSubroutineCall: subroutineSrc") subroutineReplacement)
-- flattenSubroutineCall subTable codeSeg = codeSeg
-- flattenSubroutineCall :: SubroutineTable -> Fortran Anno -> Fortran Anno
-- flattenSubroutineCall subTable codeSeg 	|	isCall = case subroutineReplacement of
-- 																		Nothing -> codeSeg
-- 																		Just flattened -> applyGlobalSrcSpan src flattened
-- 										|	otherwise = codeSeg
-- 		where 
-- 			(isCall, callExpr) = case codeSeg of
-- 							(Call _ _ expr _) -> (True, expr)
-- 							_ -> (False, error "flattenSubroutineCall")
-- 			src = srcSpan codeSeg
-- 			subroutineName = if extractVarNames callExpr == [] then (error "flattenSubroutineCall:callExpr\n" ++ (show callExpr))  else varNameStr (head (extractVarNames callExpr))
-- 			subroutineReplacement = case DMap.lookup subroutineName subTable of
-- 										Nothing -> Nothing
-- 										Just (subroutineBody, _) -> Just (substituteArguments codeSeg subroutineBody)

substituteArguments :: Fortran Anno -> ProgUnit Anno -> Fortran Anno
substituteArguments (Call _ _ _ arglist) (Sub _ _ _ _ arg (Block _ _ _ _ _ for)) = newFortran
		where
			callArgs = everything (++) (mkQ [] extractExpr_list) arglist
			bodyArgs = everything (++) (mkQ [] extractArgName) arg

			callArgs_varNames = map (\x -> if extractVarNames x == [] then error ("substituteArguments: " ++ (show x)) else  head (extractVarNames x)) callArgs
			bodyArgs_varNames = map (\(ArgName _ str) -> VarName nullAnno str) bodyArgs

			varNameReplacements = foldl (\dmap (old, new) -> DMap.insert old new dmap) DMap.empty (zip bodyArgs_varNames callArgs_varNames)

			newFortran = everywhere (mkT (replaceArgs varNameReplacements)) for

replaceArgs :: DMap.Map (VarName Anno) (VarName Anno) -> VarName Anno -> VarName Anno
replaceArgs varNameReplacements varname = DMap.findWithDefault varname varname varNameReplacements

replaceSubroutine :: SubroutineTable -> ProgUnit Anno -> ProgUnit Anno
replaceSubroutine subTable codeSeg = case codeSeg of
								(Sub _ _ _ (SubName _ str) _ _) -> fst (DMap.findWithDefault (codeSeg, "") str subTable)
								_ -> codeSeg

-- optimseBufferTransfers_subroutine :: VarAccessAnalysis -> Program Anno -> SubroutineTable -> [Program Anno]
-- optimseBufferTransfers_subroutine varAccessAnalysis ast subroutineTable = []
-- optimseBufferTransfers_subroutine :: VarAccessAnalysis -> Program Anno -> SubroutineTable -> SubroutineTable
-- optimseBufferTransfers_subroutine varAccessAnalysis mainAst subroutineTable = -- if newSubroutineTable == subroutineTable then 
-- 																				-- error ("subCalls_names: " ++ (show subCalls_names))
-- 																				-- else 
-- 																				newSubroutineTable
-- 		where
-- 			subCalls_names = extractCalledSubroutines mainAst
-- 			subCalls = foldl (\accum (subName, src) -> accum ++ [(DMap.findWithDefault (NullProg nullAnno nullSrcSpan, "") subName subroutineTable, src)]) [] subCalls_names
-- 			subCalls_handled = filter (\x -> (NullProg nullAnno nullSrcSpan, "") /= fst x) subCalls
			
-- 			newSubCalls = map (fst) (compareSubroutinesInOrder varAccessAnalysis subCalls_handled)

-- 			newSubroutineTable = foldl (\accum item -> if (DMap.lookup (extractProgUnitName item) accum) == Nothing 
-- 															then DMap.insert (extractProgUnitName item) item accum 
-- 															else error "optimseBufferTransfers_subroutine: Multiple calls to same subroutine are not supported") DMap.empty  newSubCalls

extractSubroutineInitBufferWrites :: ProgUnit Anno -> [VarName Anno]
extractSubroutineInitBufferWrites ast = fst (foldl (\(accum_r, accum_w) (item_r, item_w) -> (accum_r ++ (listSubtract item_r accum_w), accum_w ++ item_w)) ([], []) (zip reads writes))
		where
			kernels = extractKernels ast
			reads = map extractKernelReads kernels
			writes = map extractKernelWrites kernels

extractSubroutineFinalBufferReads :: ProgUnit Anno -> [VarName Anno]
extractSubroutineFinalBufferReads ast = snd (foldl (\(accum_r, accum_w) (item_r, item_w) -> (accum_r ++ item_r, accum_w ++ (listSubtract item_w accum_r))) ([], []) (zip reads writes))
		where
			kernels = extractKernels ast
			reads = reverse $ map extractKernelReads kernels
			writes = reverse $ map extractKernelWrites kernels

compareSubroutinesInOrder :: VarAccessAnalysis -> [(ProgUnit Anno, SrcSpan)] -> [(ProgUnit Anno, SrcSpan)]
compareSubroutinesInOrder varAccessAnalysis [] = []
compareSubroutinesInOrder varAccessAnalysis (subroutine:[]) = [subroutine]
compareSubroutinesInOrder varAccessAnalysis subroutines = 
														-- if newSubroutines == [] then error "compareSubroutinesInOrder: newSubroutines == []" else 
															(head newSubroutines):compareSubroutinesInOrder varAccessAnalysis (tail newSubroutines)
		where
			currentSubroutine = head subroutines
			newSubroutines = foldl (eliminateBufferPairsSubroutine_foldl varAccessAnalysis currentSubroutine) [] (tail subroutines)

eliminateBufferPairsSubroutine_foldl :: VarAccessAnalysis -> (ProgUnit Anno, SrcSpan) -> [(ProgUnit Anno, SrcSpan)] -> (ProgUnit Anno, SrcSpan) -> [(ProgUnit Anno, SrcSpan)]
eliminateBufferPairsSubroutine_foldl varAccessAnalysis firstSubroutine subroutines secondSubroutine = subroutines ++ [secondNewSubroutine]
		where
			ignoredSpans = map snd subroutines
			(firstNewSubroutine, secondNewSubroutine) = eliminateBufferPairsSubroutine varAccessAnalysis ignoredSpans firstSubroutine secondSubroutine

eliminateBufferPairsSubroutine :: VarAccessAnalysis -> [SrcSpan] -> (ProgUnit Anno, SrcSpan) -> (ProgUnit Anno, SrcSpan) -> ((ProgUnit Anno, SrcSpan),(ProgUnit Anno, SrcSpan))
eliminateBufferPairsSubroutine varAccessAnalysis ignoredSpans (firstAst, firstSrc) (secondAst, secondSrc) = 
																											-- if newFirstAst /= firstAst then  error ("elminateBuffers: " ++ (show elminateBuffers) ++
																											-- 											"\n\nfirstAst:\n" ++ (show firstAst) ++
																											-- 											"\n\nnewFirstAst:\n" ++ (show newFirstAst) ) else 
																											((newFirstAst, firstSrc), (newSecondAst, secondSrc))
		where
			(readsBetween, writesBetween) = getAccessesBetweenSrcSpansIgnore varAccessAnalysis firstSrc secondSrc ignoredSpans

			firstSubroutineReads = extractSubroutineFinalBufferReads firstAst
			secondSubroutineWrites = extractSubroutineInitBufferWrites secondAst

			elminateBuffers = listSubtract (listIntersection firstSubroutineReads secondSubroutineWrites) (readsBetween ++ writesBetween)

			firstKernels = extractKernels firstAst
			secondKernels = extractKernels secondAst

			newFirstKernels = eliminateWrites elminateBuffers firstKernels
			newSecondKernels = eliminateReads elminateBuffers secondKernels

			newFirstAst = foldl (\accum (oldF, newF) -> replaceFortran accum oldF newF) firstAst (zip firstKernels newFirstKernels)
			newSecondAst = foldl (\accum (oldF, newF) -> replaceFortran accum oldF newF) secondAst (zip secondKernels newSecondKernels)

eliminateReads :: [VarName Anno] -> [Fortran Anno] -> [Fortran Anno]
eliminateReads [] kernelList = kernelList
eliminateReads eliminateList [] = error "eliminateReads\': How did it come to this?"
eliminateReads eliminateList kernelList = newKernel:(eliminateReads newEliminateList (tail kernelList))
		where
			curerentKernel = head kernelList
			bufferReads = extractKernelWrites curerentKernel
			newReads = listSubtract bufferReads eliminateList
			newEliminateList = listSubtract eliminateList bufferReads
			newKernel = replaceKernelWrites curerentKernel newReads

eliminateWrites :: [VarName Anno] -> [Fortran Anno] -> [Fortran Anno]
eliminateWrites eliminateList kernelList = reverse (eliminateWrites' eliminateList (reverse kernelList))

eliminateWrites' :: [VarName Anno] -> [Fortran Anno] -> [Fortran Anno]
eliminateWrites' [] kernelList = kernelList
eliminateWrites' eliminateList [] = error "eliminateWrites\': How did it come to this?"
eliminateWrites' eliminateList kernelList = newKernel:(eliminateWrites' newEliminateList (tail kernelList))
		where
			curerentKernel = head kernelList
			bufferWrites = extractKernelReads curerentKernel
			newWrites = listSubtract bufferWrites eliminateList
			newEliminateList = listSubtract eliminateList bufferWrites
			newKernel = replaceKernelReads curerentKernel newWrites

optimiseBufferTransfers :: SubroutineTable -> Program Anno -> (SubroutineTable, (([VarName Anno], SrcSpan), ([VarName Anno], SrcSpan)))
optimiseBufferTransfers subTable mainAst = 
											-- if newSubTable == subTable 
											-- then
											-- 	error ("optimiseBufferTransfers, newSubTable == subTable\nmatches: " ++ (show matches)
											-- 			++ "\n\n(DMap.keys subTable): " ++ (show (DMap.keys subTable))
											-- 			)
											-- else
												(newSubTable,((initWrites, earliestInitSrc), (tearDownReads, latestTearDownSrc)))
		where
			flattenedAst = flattenSubroutineAppearences subTable mainAst
			flattenedVarAccessAnalysis = analyseAllVarAccess flattenedAst
			optimisedFlattenedAst = optimseBufferTransfers_kernel flattenedVarAccessAnalysis flattenedAst

			varAccessAnalysis = analyseAllVarAccess mainAst
			kernels_optimisedBetween = extractKernels optimisedFlattenedAst
			
			kernelRangeSrc = generateKernelCallSrcRange subTable mainAst
			-- kernelRangeSrc = (fst (srcSpan (head kernels_optimisedBetween)), snd (srcSpan (last kernels_optimisedBetween)))
			(kernels_withoutInitOrTearDown, initWrites, tearDownReads) = stripInitAndTearDown flattenedVarAccessAnalysis kernelRangeSrc kernels_optimisedBetween

			earliestInitSrc = generateSrcSpan "" (findEarliestInitialisationSrcSpan varAccessAnalysis kernelRangeSrc initWrites)
			latestTearDownSrc = shiftSrcSpan (-1) (generateSrcSpan "" (findLatestTearDownSrcSpan varAccessAnalysis kernelRangeSrc tearDownReads))

			-- ast_withoutInitOrTearDown = foldl (\accumAst (oldFortran, newFortran) -> replaceFortran accumAst oldFortran newFortran) ast_optimisedBetweenKernels (zip kernels_optimisedBetween kernels_withoutInitOrTearDown)

			oldKernels = extractKernels flattenedAst
			-- optimisedKernels = extractKernels optimisedFlattenedAst
			-- kernelPairs = zip oldKernels optimisedKernels
			kernelPairs = zip oldKernels kernels_withoutInitOrTearDown
			matches = map (\(a, b) -> a == b) kernelPairs
			keys = (DMap.keys subTable)

			newSubTable = foldl (replaceKernels_foldl kernelPairs) subTable (DMap.keys subTable)
			-- newSubTable = foldl (\dmap subName -> DMap.insert subName (replaceKernels kernelPairs (DMap.findWithDefault (error "optimiseBufferTransfers") subName subTable)) dmap) DMap.empty (DMap.keys subTable)

generateKernelCallSrcRange :: SubroutineTable -> Program Anno -> SrcSpan
generateKernelCallSrcRange subTable ast = (kernelsStart, kernelsEnd)
		where
			callsAndStrings = everything (++) (mkQ [] extractCallsWithStrings) ast
			callSrcs = map (\(callCode, callString) -> srcSpan callCode) (filter (\(callCode, callString) -> elem callString (DMap.keys subTable)) callsAndStrings)
			kernelsStart = fst (fromMaybe (error "generateKernelCallSrcRange") (getEarliestSrcSpan callSrcs))
			kernelsEnd = snd (fromMaybe (error "generateKernelCallSrcRange") (getLatestSrcSpan callSrcs))

-- varNameStr (head (extractVarNames callExpr))
-- calls = everything (++) (mkQ [] extractCalls) mainAst

replaceKernels_foldl :: [(Fortran Anno, Fortran Anno)] -> SubroutineTable -> String -> SubroutineTable
replaceKernels_foldl kernelPairs subTable subName = DMap.insert subName (newAst, filename) subTable
		where
			(ast, filename) = DMap.findWithDefault (error "replaceKernels_foldl") subName subTable
			-- newAst = replaceKernels (zip (map (fst) kernelPairs) (reverse (map (fst) kernelPairs))) ast
			newAst = replaceKernels kernelPairs ast

replaceKernels :: [(Fortran Anno, Fortran Anno)] -> ProgUnit Anno -> ProgUnit Anno
replaceKernels kernelPairs subroutine = foldl (\accumSub (old, optim) -> replaceFortran accumSub old optim) subroutine kernelPairs

optimseBufferTransfers_kernel :: VarAccessAnalysis -> Program Anno -> Program Anno
optimseBufferTransfers_kernel varAccessAnalysis ast = 
														-- if newAst == ast 
														-- then
															-- error ("\ndebugStr: " ++ debugStr ++ "\n\nKERNELS:\n" ++ show kernels) 
															-- error ("initWrites: " ++ (show initWrites) ++ "\n\ntearDownReads: " ++ (show tearDownReads))
														-- else
															ast_optimisedBetweenKernels
															-- ast_withoutInitOrTearDown
		where
			(ast_optimisedBetweenKernels, debugStr) = compareKernelsInOrder varAccessAnalysis kernels (ast, "")
			kernels = extractKernels ast
			kernels_optimisedBetween = extractKernels ast_optimisedBetweenKernels

			-- kernelRangeSrc = (fst (srcSpan (head kernels_optimisedBetween)), snd (srcSpan (last kernels_optimisedBetween)))
			-- (kernels_withoutInitOrTearDown, initWrites, tearDownReads) = stripInitAndTearDown varAccessAnalysis kernelRangeSrc kernels_optimisedBetween

			-- ast_withoutInitOrTearDown = foldl (\accumAst (oldFortran, newFortran) -> replaceFortran accumAst oldFortran newFortran) ast_optimisedBetweenKernels (zip kernels_optimisedBetween kernels_withoutInitOrTearDown)

findEarliestInitialisationSrcSpan :: VarAccessAnalysis -> SrcSpan -> [VarName Anno] -> SrcSpan
findEarliestInitialisationSrcSpan varAccessAnalysis kernelsRange initWrites = lastWrite
		where
			writesBefore = foldl (\accum item -> accum ++ snd (getAccessLocationsBeforeSrcSpan varAccessAnalysis item kernelsRange)) [] initWrites
			lastWrite = fromMaybe nullSrcSpan (getLatestSrcSpan writesBefore)

findLatestTearDownSrcSpan :: VarAccessAnalysis -> SrcSpan -> [VarName Anno] -> SrcSpan
findLatestTearDownSrcSpan varAccessAnalysis kernelsRange tearDownReads = firstRead
		where
			readsAfter = foldl (\accum item -> accum ++ fst (getAccessLocationsAfterSrcSpan varAccessAnalysis item kernelsRange)) [] tearDownReads
			firstRead = fromMaybe nullSrcSpan (getEarliestSrcSpan readsAfter)


stripInitAndTearDown :: VarAccessAnalysis -> SrcSpan -> [Fortran Anno] -> ([Fortran Anno], [VarName Anno], [VarName Anno])
stripInitAndTearDown _ _ [] = ([], [], [])
stripInitAndTearDown varAccessAnalysis (kernelsStart, kernelsEnd) kernels = (newCurrentKernel:recursiveKernels, initialisingWrites ++ recursiveInitialisingWrites, tearDownReads ++ recursiveTearDownReads)
		where
			currentKernel = head kernels
			(currentSrcStart, currentSrcEnd) = srcSpan currentKernel
			bufferWrites = extractKernelReads currentKernel -- buffer writes are for variables that are read by the kernel
			bufferReads = extractKernelWrites currentKernel -- buffer reads are for variables that are written to by the kernel
			(_, writesBeforeCurrentKernel) = getAccessesBetweenSrcSpans varAccessAnalysis kernelsStart currentSrcStart
			(readsAfterCurrentKernel, _) = getAccessesBetweenSrcSpans varAccessAnalysis currentSrcEnd kernelsEnd

			initialisingWrites = listSubtract bufferWrites writesBeforeCurrentKernel
			tearDownReads = listSubtract bufferReads readsAfterCurrentKernel

			newBufferWrites = listSubtract bufferWrites initialisingWrites
			newBufferReads = listSubtract bufferReads tearDownReads
			
			newCurrentKernel = replaceKernelWrites (replaceKernelReads currentKernel newBufferWrites) newBufferReads

			(recursiveKernels, recursiveInitialisingWrites, recursiveTearDownReads) = stripInitAndTearDown varAccessAnalysis (kernelsStart, kernelsEnd) (tail kernels)

compareKernelsInOrder :: VarAccessAnalysis -> [Fortran Anno] -> (Program Anno, String) -> (Program Anno, String)
-- compareKernelsInOrder :: VarAccessAnalysis -> [Fortran Anno] -> Program Anno -> Program Anno
compareKernelsInOrder varAccessAnalysis [] ast = ast
compareKernelsInOrder varAccessAnalysis kernels (ast, s) = compareKernelsInOrder varAccessAnalysis (newKernels) (newAst, s ++ debugStr)
		where
			currentKernel = head kernels
			-- (_, newAst, _, debugStr) = foldl (eliminateBufferPairsKernel_foldl varAccessAnalysis) (currentKernel, ast, [], "") (tail kernels)
			(newFirstKernel, newKernels, debugStr) = eliminateBufferPairsKernel_recurse varAccessAnalysis currentKernel (tail kernels) [] ""
			newAst = foldl (\accumAst (oldFortran, newFortran) -> replaceFortran accumAst oldFortran newFortran) ast (zip kernels (newFirstKernel:newKernels))

eliminateBufferPairsKernel_recurse :: VarAccessAnalysis -> Fortran Anno -> [Fortran Anno] -> [SrcSpan] -> String -> (Fortran Anno, [Fortran Anno], String)
eliminateBufferPairsKernel_recurse varAccessAnalysis firstKernel [] ignoredSpans debug =  (firstKernel, [], "")
eliminateBufferPairsKernel_recurse varAccessAnalysis firstKernel kernels ignoredSpans debug = (resursiveCall_firstKernel, newSecondKernel:resursiveCall_kernels, debugStr ++ resursiveCall_debugStr)
		where
			secondKernel = head kernels
			(newFirstKernel, newSecondKernel, debugStr) = eliminateBufferPairsKernel varAccessAnalysis ignoredSpans firstKernel secondKernel
			(resursiveCall_firstKernel, resursiveCall_kernels, resursiveCall_debugStr) = eliminateBufferPairsKernel_recurse varAccessAnalysis newFirstKernel (tail kernels) ((srcSpan secondKernel):ignoredSpans) debugStr	

eliminateBufferPairsKernel :: VarAccessAnalysis -> [SrcSpan] -> Fortran Anno -> Fortran Anno -> (Fortran Anno, Fortran Anno, String)
eliminateBufferPairsKernel varAccessAnalysis ignoredSpans firstKernel secondKernel = (newFirstKernel, newSecondKernel, debugStr)
		where
			firstKernel_src = srcSpan firstKernel
			secondKernel_src = srcSpan secondKernel

			(readsBetween, writesBetween) = getAccessesBetweenSrcSpansIgnore varAccessAnalysis firstKernel_src secondKernel_src ignoredSpans

			-- 	Rather confusingly, the reads I am refering to here are the buffer reads that must occur 
			--	AFTER the kernel call and NOT the arguments that are 'read' by the kernel. As it turns out,
			--	the variables that I am interested in are the variables that the kernel views as 'writes'.
			--	The equivalent is true for the secondBufferWrites variable. Not the written arguments but 
			-- 	the buffers that must be written to before the start of the kernel.
			firstBufferReads = extractKernelWrites firstKernel
			firstBufferWrites = extractKernelReads firstKernel
			secondBufferReads = extractKernelWrites secondKernel
			secondBufferWrites = extractKernelReads secondKernel
			--	SIMPLE CASE wihtout any analysis between kernels
			--		newFirstBufferReads = listSubtract firstBufferReads secondBufferWrites
			--		newsecondBufferWrites = listSubtract secondBufferWrites firstBufferReads

			--	More complex analysis would differentiate between reads and writes between kernels.
			-- newFirstBufferReads = listSubtractWithExemption (readsBetween ++ writesBetween) firstBufferReads secondBufferWrites
			-- newSecondBufferWrites = listSubtractWithExemption (readsBetween ++ writesBetween) secondBufferWrites firstBufferReads 

			newSecondBufferWrites_preCrossOver = listSubtractWithExemption (writesBetween) secondBufferWrites firstBufferWrites 
			newSecondBufferWrites = listSubtractWithExemption (writesBetween) newSecondBufferWrites_preCrossOver firstBufferReads
			newFirstBufferReads_crossOver = listSubtractWithExemption (readsBetween) firstBufferReads newSecondBufferWrites
			newFirstBufferReads = listSubtractWithExemption (readsBetween) newFirstBufferReads_crossOver secondBufferReads

			-- newFirstBufferReads = listSubtractWithExemption ([]) firstBufferReads secondBufferWrites
			-- newSecondBufferWrites = listSubtractWithExemption ([]) secondBufferWrites firstBufferReads 

			k1src = errorLocationFormatting (srcSpan firstKernel)
			k2src = errorLocationFormatting (srcSpan secondKernel)

			debugStr = k1src ++ " firstBufferReads: " ++ (show firstBufferReads) ++ "\n" ++ k2src ++ " secondBufferWrites: " ++ (show secondBufferWrites) 
						++ "\n" ++ k1src ++ " newFirstBufferReads: " ++ (show newFirstBufferReads) ++ "\n" ++ k2src ++ " newSecondBufferWrites: " ++ (show newSecondBufferWrites) ++ "\n\n"

			newFirstKernel = replaceKernelWrites firstKernel newFirstBufferReads
			newSecondKernel = replaceKernelReads secondKernel newSecondBufferWrites

-- replaceFortran :: Program Anno -> Fortran Anno -> Fortran Anno -> Program Anno
replaceFortran progAst oldFortran newFortran = everywhere (mkT (replaceFortran' oldFortran newFortran)) progAst

replaceFortran' :: Fortran Anno -> Fortran Anno -> Fortran Anno -> Fortran Anno
replaceFortran' oldFortran newFortran currentFortran 	|	(applyGeneratedSrcSpans oldFortran) == (applyGeneratedSrcSpans currentFortran) = normaliseSrcSpan currentFortran newFortran
														|	otherwise = currentFortran

replaceProgUnit ast oldProgUnit newProgUnit = everywhere (mkT (replaceProgUnit' oldProgUnit newProgUnit)) ast 

replaceProgUnit' :: ProgUnit Anno -> ProgUnit Anno -> ProgUnit Anno -> ProgUnit Anno
replaceProgUnit' oldProgUnit newProgUnit currentProgUnit 	| 	(applyGeneratedSrcSpans oldProgUnit) == (applyGeneratedSrcSpans currentProgUnit) = normaliseSrcSpan currentProgUnit newProgUnit
															|	otherwise = currentProgUnit

extractKernelReads :: Fortran Anno -> [VarName Anno]
extractKernelReads codeSeg = case codeSeg of
				OpenCLMap _ _ reads _ _ _ -> reads
				OpenCLReduce _ _ reads _ _ _ _ -> reads
				_ -> error "extractKernelReads: not a kernel"

extractKernelWrites :: Fortran Anno -> [VarName Anno]
extractKernelWrites codeSeg = case codeSeg of
				OpenCLMap _ _ _ writes _ _ -> writes
				OpenCLReduce _ _ _ writes _ _ _ -> writes
				_ -> error "extractKernelWrites: not a kernel"

replaceKernelReads :: Fortran Anno -> [VarName Anno] -> Fortran Anno
replaceKernelReads codeSeg newReads = case codeSeg of
				OpenCLMap anno src reads writes loopV fortran -> OpenCLMap anno src newReads writes loopV fortran
				OpenCLReduce anno src reads writes loopV redV fortran -> OpenCLReduce anno src newReads writes loopV redV fortran
				_ -> error "replaceKernelReads: not a kernel"

replaceKernelWrites :: Fortran Anno -> [VarName Anno] -> Fortran Anno
replaceKernelWrites codeSeg newWrites = case codeSeg of
				OpenCLMap anno src reads writes loopV fortran -> OpenCLMap anno src reads newWrites loopV fortran
				OpenCLReduce anno src reads writes loopV redV fortran -> OpenCLReduce anno src reads newWrites loopV redV fortran
				_ -> error "replaceKernelWrites: not a kernel"

constructSubroutineTable :: [(Program Anno, String)] -> SubroutineTable
constructSubroutineTable programs = foldl (\accum (ast, filename) -> DMap.insert (extractProgUnitName ast) (ast, filename) accum) DMap.empty parsedSubroutines
		where
			parsedSubroutines = foldl (\accum (ast, filename) -> accum ++ (map (\x -> (x, filename)) (extractSubroutines ast))) [] programs

extractProgUnitName :: ProgUnit Anno -> String
extractProgUnitName ast 	|	subNames == [] = error ((show ast) ++ "\n\nextractProgUnitName: no subNames")
							|	otherwise = extractStringFromSubName (head subNames)
		where
			subNames = everything (++) (mkQ [] getSubNames) ast

extractCalledSubroutines :: Program Anno -> [(String, SrcSpan)]
extractCalledSubroutines ast = everything (++) (mkQ [] extractCalledSubroutines_fortran) ast

extractCalledSubroutines_fortran :: Fortran Anno -> [(String, SrcSpan)]
extractCalledSubroutines_fortran (Call _ src expr _) 	|	isVar expr = map (\x -> (varNameStr x, src)) (extractVarNames expr)
														|	otherwise = error "extractCalledSubroutines_fortran: not var"
extractCalledSubroutines_fortran _ = []

extractStringFromSubName :: SubName Anno -> String
extractStringFromSubName (SubName _ str) = str

extractSubroutines ast = everything (++) (mkQ [] extractSubroutines') ast

extractSubroutines' :: ProgUnit Anno -> [ProgUnit Anno]
extractSubroutines' codeSeg = case codeSeg of
								(Sub _ _ _ _ _ _) -> [codeSeg]
								_ -> []