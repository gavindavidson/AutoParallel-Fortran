module BufferTransferAnalysis where

import LanguageFortranTools
import VarAccessAnalysis
import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran
import qualified Data.Map as DMap

type SubroutineTable = DMap.Map String (ProgUnit Anno)

replaceSubroutineAppearences :: SubroutineTable -> [Program Anno] -> [Program Anno]
replaceSubroutineAppearences subTable [] = []
replaceSubroutineAppearences subTable (firstProgram:programs) = updated:replaceSubroutineAppearences subTable programs
		where 
			updated = everywhere (mkT (replaceSubroutine subTable)) firstProgram

replaceSubroutine :: SubroutineTable -> ProgUnit Anno -> ProgUnit Anno
replaceSubroutine subTable codeSeg = case codeSeg of
								(Sub _ _ _ (SubName _ str) _ _) -> DMap.findWithDefault codeSeg str subTable
								_ -> codeSeg

-- optimseBufferTransfers_subroutine :: VarAccessAnalysis -> Program Anno -> SubroutineTable -> [Program Anno]
-- optimseBufferTransfers_subroutine varAccessAnalysis ast subroutineTable = []
optimseBufferTransfers_subroutine :: VarAccessAnalysis -> Program Anno -> SubroutineTable -> SubroutineTable
optimseBufferTransfers_subroutine varAccessAnalysis mainAst subroutineTable = if newSubroutineTable == subroutineTable then error "optimseBufferTransfers_subroutine: newSubroutineTable == subroutineTable" else newSubroutineTable
		where
			subCalls_names = extractCalledSubroutines mainAst
			subCalls = foldl (\accum (subName, src) -> accum ++ [(DMap.findWithDefault (NullProg nullAnno nullSrcSpan) subName subroutineTable, src)]) [] subCalls_names
			subCalls_handled = filter (\x -> (NullProg nullAnno nullSrcSpan) /= fst x) subCalls
			
			newSubCalls = map (fst) (compareSubroutinesInOrder varAccessAnalysis subCalls_handled)

			newSubroutineTable = foldl (\accum item -> if (DMap.lookup (extractProgUnitName item) accum) == Nothing 
															then DMap.insert (extractProgUnitName item) item accum 
															else error "optimseBufferTransfers_subroutine: Multiple calls to same subroutine are not supported") DMap.empty  newSubCalls

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
eliminateWrites eliminateList kernelList = eliminateWrites' eliminateList (reverse kernelList)

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
			let parsedPrograms' = foldl (\accum item -> accum ++ extractSubroutines item) [] parsedPrograms
			-- mapM (\x -> putStr ((show x) ++ "\n\n")) parsedPrograms'
			let subTable = foldl (\accum item -> DMap.insert (extractProgUnitName item) item accum) DMap.empty parsedPrograms'
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