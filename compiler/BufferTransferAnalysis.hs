module BufferTransferAnalysis where

import LanguageFortranTools
import VarAccessAnalysis
import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran
import qualified Data.Map as DMap

type SubroutineTable = DMap.Map String (ProgUnit Anno, String)

replaceSubroutineAppearences :: SubroutineTable -> [Program Anno] -> [Program Anno]
replaceSubroutineAppearences subTable [] = []
replaceSubroutineAppearences subTable (firstProgram:programs) = updated:replaceSubroutineAppearences subTable programs
		where 
			updated = everywhere (mkT (replaceSubroutine subTable)) firstProgram

flattenSubroutineAppearences :: SubroutineTable -> Program Anno -> Program Anno
flattenSubroutineAppearences subTable mainAst = updated
		where
			subroutines = DMap.keys subTable
			updated = everywhere (mkT (flattenSubroutineCall subTable)) mainAst

flattenSubroutineCall :: SubroutineTable -> Fortran Anno -> Fortran Anno
flattenSubroutineCall subTable codeSeg 	|	isCall = case subroutineReplacement of
																		Nothing -> codeSeg
																		Just flattened -> flattened
										|	otherwise = codeSeg
		where 
			(isCall, callExpr) = case codeSeg of
							(Call _ _ expr _) -> (True, expr)
							_ -> (False, error "flattenSubroutineCall")
			subroutineName = if extractVarNames callExpr == [] then (error "flattenSubroutineCall:callExpr\n" ++ (show callExpr))  else varNameStr (head (extractVarNames callExpr))
			subroutineReplacement = case DMap.lookup subroutineName subTable of
										Nothing -> Nothing
										Just (subroutineBody, _) -> Just (substituteArguments codeSeg subroutineBody)

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
			(readsBetweem, writesBetween) = getAccessesBetweenSrcSpansIgnore varAccessAnalysis firstSrc secondSrc ignoredSpans

			firstSubroutineReads = extractSubroutineFinalBufferReads firstAst
			secondSubroutineWrites = extractSubroutineInitBufferWrites secondAst

			elminateBuffers = listSubtract (listIntersection firstSubroutineReads secondSubroutineWrites) (readsBetweem ++ writesBetween)

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

optimiseBufferTransfers :: SubroutineTable -> Program Anno -> SubroutineTable
optimiseBufferTransfers subTable mainAst = newSubTable
		where
			flattenedAst = flattenSubroutineAppearences subTable mainAst
			varAccessAnalysis = analyseAllVarAccess flattenedAst
			optimisedFlattenedAst = optimseBufferTransfers_kernel varAccessAnalysis flattenedAst

			oldKernels = extractKernels flattenedAst
			optimisedKernels = extractKernels optimisedFlattenedAst
			kernelPairs = zip oldKernels optimisedKernels

			newSubTable = foldl (replaceKernels_foldl kernelPairs) subTable (DMap.keys subTable)
			-- newSubTable = foldl (\dmap subName -> DMap.insert subName (replaceKernels kernelPairs (DMap.findWithDefault (error "optimiseBufferTransfers") subName subTable)) dmap) DMap.empty (DMap.keys subTable)

replaceKernels_foldl :: [(Fortran Anno, Fortran Anno)] -> SubroutineTable -> String -> SubroutineTable
replaceKernels_foldl kernelPairs subTable subName = DMap.insert subName (newAst, filename) subTable
		where
			(ast, filename) = DMap.findWithDefault (error "replaceKernels_foldl") subName subTable
			newAst = replaceKernels kernelPairs ast

replaceKernels :: [(Fortran Anno, Fortran Anno)] -> ProgUnit Anno -> ProgUnit Anno
replaceKernels kernelPairs subroutine = foldl (\accumSub (old, optim) -> replaceFortran accumSub old optim) subroutine kernelPairs

optimseBufferTransfers_kernel :: VarAccessAnalysis -> Program Anno -> Program Anno
optimseBufferTransfers_kernel varAccessAnalysis ast = compareKernelsInOrder varAccessAnalysis kernels ast
		where
			kernels = extractKernels ast

compareKernelsInOrder :: VarAccessAnalysis -> [Fortran Anno] -> Program Anno -> Program Anno
compareKernelsInOrder varAccessAnalysis [] ast = ast
compareKernelsInOrder varAccessAnalysis kernels ast = compareKernelsInOrder varAccessAnalysis (tail kernels) newAst
		where
			currentKernel = head kernels
			(newAst, _) = foldl (eliminateBufferPairsKernel_foldl varAccessAnalysis currentKernel) (ast, []) (tail kernels)

eliminateBufferPairsKernel_foldl :: VarAccessAnalysis -> Fortran Anno -> (Program Anno, [SrcSpan]) -> Fortran Anno -> (Program Anno, [SrcSpan])
eliminateBufferPairsKernel_foldl varAccessAnalysis firstKernel (ast, ignoredSpans) secondKernel = (astWithNewSecondKernel, ignoredSpans ++ [srcSpan secondKernel])
		where
			(newFirstKernel, newSecondKernel) = eliminateBufferPairsKernel varAccessAnalysis ignoredSpans firstKernel secondKernel
			astWithNewFirstKernel = replaceFortran ast firstKernel newFirstKernel
			astWithNewSecondKernel = replaceFortran astWithNewFirstKernel secondKernel newSecondKernel

eliminateBufferPairsKernel :: VarAccessAnalysis -> [SrcSpan] -> Fortran Anno -> Fortran Anno -> (Fortran Anno, Fortran Anno)
eliminateBufferPairsKernel varAccessAnalysis ignoredSpans firstKernel secondKernel = (newFirstKernel, newsecondKernel)
		where
			firstKernel_src = srcSpan firstKernel
			secondKernel_src = srcSpan secondKernel

			(readsBetweem, writesBetween) = getAccessesBetweenSrcSpansIgnore varAccessAnalysis firstKernel_src secondKernel_src ignoredSpans

			-- 	Rather confusingly, the reads I am refering to here are the buffer reads that must occur 
			--	AFTER the kernel call and NOT the arguments that are 'read' by the kernel. As it turns out,
			--	the variables that I am interested in are the variables that the kernel views as 'writes'.
			--	The equivalent is true for the secondBufferWrites variable. Not the written arguments but 
			-- 	the buffers that must be written to before the start of the kernel.
			firstBufferReads = extractKernelWrites firstKernel
			secondBufferWrites = extractKernelReads secondKernel
			--	SIMPLE CASE wihtout any analysis between kernels
			--		newFirstBufferReads = listSubtract firstBufferReads secondBufferWrites
			--		newsecondBufferWrites = listSubtract secondBufferWrites firstBufferReads

			--	More complex analysis would differentiate between reads and writes between kernels.
			newFirstBufferReads = listSubtractWithExemption (readsBetweem ++ writesBetween) firstBufferReads secondBufferWrites
			newSecondBufferWrites = listSubtractWithExemption (readsBetweem ++ writesBetween) secondBufferWrites firstBufferReads 

			newFirstKernel = replaceKernelWrites firstKernel newFirstBufferReads
			newsecondKernel = replaceKernelReads secondKernel newSecondBufferWrites

-- replaceFortran :: Program Anno -> Fortran Anno -> Fortran Anno -> Program Anno
replaceFortran progAst oldFortran newFortran = everywhere (mkT (replaceFortran' oldFortran newFortran)) progAst

replaceFortran' :: Fortran Anno -> Fortran Anno -> Fortran Anno -> Fortran Anno
replaceFortran' oldFortran newFortran currentFortran 	|	oldFortran == currentFortran = newFortran
														|	otherwise = currentFortran

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

parseProgUnits :: [String] -> [String] -> IO(SubroutineTable)
parseProgUnits cppDFlags filenames = do
			parsedPrograms <- mapM (parseFile cppDFlags) filenames
			let parsedPrograms' = foldl (\accum (ast, filename) -> accum ++ (map (\x -> (x, filename)) (extractSubroutines ast))) [] (zip parsedPrograms filenames)
			-- mapM (\x -> putStr ((show x) ++ "\n\n")) parsedPrograms'
			let subTable = foldl (\accum (ast, filename) -> DMap.insert (extractProgUnitName ast) (ast, filename) accum) DMap.empty parsedPrograms'

			-- let subroutineNamePairs = foldl (\accum (ast, fn) -> accum ++ (map (\x -> (extractProgUnitName x, fn)) (extractSubroutines ast))) [] (zip parsedPrograms filenames)

			return subTable

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