module BufferTransferAnalysis where

import LanguageFortranTools
import VarAccessAnalysis
import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran

optimseBufferTransfers :: VarAccessAnalysis -> Program Anno -> Program Anno
optimseBufferTransfers varAccessAnalysis ast = compareKernelsInOrder varAccessAnalysis kernels ast
		where
			kernels = extractKernels ast

compareKernelsInOrder :: VarAccessAnalysis -> [Fortran Anno] -> Program Anno -> Program Anno
compareKernelsInOrder varAccessAnalysis [] ast = ast
compareKernelsInOrder varAccessAnalysis kernels ast = compareKernelsInOrder varAccessAnalysis (tail kernels) newAst
		where
			currentKernel = head kernels
			(newAst, _) = foldl (eliminateBufferPairs_foldl varAccessAnalysis currentKernel) (ast, []) (tail kernels)

eliminateBufferPairs_foldl :: VarAccessAnalysis -> Fortran Anno -> (Program Anno, [SrcSpan]) -> Fortran Anno -> (Program Anno, [SrcSpan])
eliminateBufferPairs_foldl varAccessAnalysis firstKernel (ast, ignoredSpans) secondKernel = (astWithNewSecondKernel, ignoredSpans ++ [srcSpan secondKernel])
		where
			(newFirstKernel, newSecondKernel) = eliminateBufferPairs varAccessAnalysis ignoredSpans firstKernel secondKernel
			astWithNewFirstKernel = replaceFortran ast firstKernel newFirstKernel
			astWithNewSecondKernel = replaceFortran astWithNewFirstKernel secondKernel newSecondKernel

eliminateBufferPairs :: VarAccessAnalysis -> [SrcSpan] -> Fortran Anno -> Fortran Anno -> (Fortran Anno, Fortran Anno)
eliminateBufferPairs varAccessAnalysis ignoredSpans firstKernel secondKernel = (newFirstKernel, newsecondKernel)
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
			newsecondKernel = replaceKernelReads secondKernel secondBufferWrites

replaceFortran :: Program Anno -> Fortran Anno -> Fortran Anno -> Program Anno
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