module BufferTransferAnalysis where

-- 	This module deals with optimising the use of buffer reads and writes in an attempt to minimise memory transfers. The functions
-- 	produce versions of the AST nodes of OpenCLMap and OpenCLReduce kernels where written and read arguments are removed if they
-- 	are deemed surplus. The new versions of the AST nodes are then passed back to the main which in turn passes them to code emission.
-- 	Also, this module produces a set of Data.Maps that contain argument/varname translations. That is, in situations where the same
-- 	variable is used with different varnames, there is a way to determine whether two differntly named vars from different subroutines
-- 	are in fact the same variable. This is necessary for correct buffer accesses across different subroutines

import LanguageFortranTools
import VarAccessAnalysis
import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Data.Maybe
import Language.Fortran
import LanguageFortranTools
import qualified Data.Map as DMap

-- 	This datastructure holds subroutines, with the names of the subrouines as the key. The stored item is the AST of the subroutine
--	and the name of the file from which the subroutine was orignally defined.
type SubroutineTable = DMap.Map String (ProgUnit Anno, String)
subroutineTable_ast (a, _) =  a
subroutineTable_filename (_, b) =  b

--	These datastructures facilitate argument/variable translation. For each subroutine there is an 'ArgumentTranslation' structure
--	stored in the 'ArgumentTranslationSubroutines' structure. Each 'ArgumentTranslation' structure contains a set of VarName keys
--	and related VarName items. The VarName keys are the variables that appear in the current subroutine and the related items are
--	the names of the variables in terms of the main method (or whatever context the structure is constructed with). The structure
--	means that when a buffer is referenced in a subroutine in the final output code, first a check to this structure is made to be
--	sure that the variable in question doesn't exist under a different name/buffer number.
type ArgumentTranslationSubroutines = DMap.Map (String) ArgumentTranslation
type ArgumentTranslation = DMap.Map (VarName Anno) (VarName Anno)
emptyArgumentTranslation = DMap.empty

--	This is one of the entry points into this module from the main. Using the previously constructed subroutine table and argument translation table(s), 
--	the function tries to preen the buffer transfers that take place. 
--	BASIC STEPS:
--		-	Flatten the main AST by replacing any call to known* subroutines with body of the already parallelised subroutine. This step involves adjusting
--			source location information such as line numbers so that variable access analysis can be correclty performed and utilised. (*known here means that
--			the subroutine body was provided in one of the input files to the compiler),
--		-	On the new flattened main AST, perform variable access analysis to catalogue where and how variables are accessed across the entire codebase. This
--			information allows the compiler to determine whether or not a variable is used between successive kernel calls and therefore whether it needs to be
--			read back to the host or written to the device.
--		-	Using variable access analysis, remove as many arguments as possible from all of the kernels that have been created.
--		-	Take the new kernels and determine when the 'initialisation' subroutine call can be perfomed. This is done with 'findEarliestInitialisationSrcSpan'
--		-	Finally, replace the kernels in the original subroutine table with the new kernels that use far less read/write arguments. Return the new
--			subroutine table along with the SrcSpans that indicate where initialisation should happen and where the OpenCL portion of the main ends.
optimiseBufferTransfers :: SubroutineTable -> ArgumentTranslationSubroutines -> Program Anno -> (SubroutineTable, Program Anno)
optimiseBufferTransfers subTable argTranslations mainAst = (newSubTable, 
														 		newMainAst_withReadsWrites)
		where
			flattenedAst = flattenSubroutineAppearences subTable argTranslations mainAst
			flattenedVarAccessAnalysis = analyseAllVarAccess flattenedAst
			optimisedFlattenedAst = optimseBufferTransfers_kernel flattenedVarAccessAnalysis flattenedAst

			varAccessAnalysis = analyseAllVarAccess mainAst
			kernels_optimisedBetween = extractKernels optimisedFlattenedAst
			
			(kernelStartSrc, kernelEndSrc) = generateKernelCallSrcRange subTable mainAst

			kernelRangeSrc = (fst kernelStartSrc, snd kernelEndSrc)
			(kernels_withoutInitOrTearDown, initWrites, varsOnDeviceAfterOpenCL) = stripInitAndTearDown flattenedVarAccessAnalysis kernelRangeSrc kernels_optimisedBetween

			readConsiderationSrc = kernelRangeSrc

			(bufferWritesBefore, bufferWritesAfter) = generateBufferInitPositions initWrites mainAst kernelStartSrc kernelEndSrc varAccessAnalysis
			(bufferReadsBefore, bufferReadsAfter) = generateBufferReadPositions varsOnDeviceAfterOpenCL mainAst kernelStartSrc kernelEndSrc varAccessAnalysis

			newMainAst_withReads = insertBufferReadsBefore bufferReadsBefore (insertBufferReadsAfter bufferReadsAfter mainAst)
			newMainAst_withReadsWrites = insertBufferWritesBefore bufferWritesBefore (insertBufferWritesAfter bufferWritesAfter newMainAst_withReads)

			oldKernels = extractKernels flattenedAst
			kernelPairs = zip oldKernels kernels_withoutInitOrTearDown
			matches = map (\(a, b) -> a == b) kernelPairs
			keys = (DMap.keys subTable)

			newSubTable = foldl (replaceKernels_foldl kernelPairs) subTable (DMap.keys subTable)

--	AIM:
--		Produce a list of varnames and associated locations where the location is the position at which that var's buffer must be read to allow the host
--		program to function correctly. This must consider loops.
--	LOGIC:
--		Construct an SrcSpan that represents the part of the program that would affect the values written to OpenCL buffers PRIOR to OpenCL doing work.
--		Inside this range, (which includes whole loops if the openCl work starts in a loop), locations where the vars in question are written to are
--		picked out. Those locations that are null (there are no writes in range) are removed. Next, only latests locations for each var are saved.
--		Finally, if those locations are inside the consideration range, but AFTER the end of the kernel calls (as in, we are in a loop, and the write
--		happens after the kennel calls in the loop) then the write location must be just before the start of the kernel calls, to ensure correctness.
generateBufferInitPositions :: [VarName Anno] -> Program Anno -> SrcSpan -> SrcSpan -> VarAccessAnalysis -> ([(VarName Anno, SrcSpan)], [(VarName Anno, SrcSpan)])
generateBufferInitPositions initWrites mainAst kernelStartSrc kernelEndSrc varAccessAnalysis = (bufferWritesBeforeSrc, bufferWritesAfterSrc)
		where 
			fortranNode = (extractFirstFortran mainAst)
			writeConsiderationEnd = findWriteConsiderationEnd (fst kernelStartSrc) fortranNode
			writeConsiderationSrc = (fst (srcSpan fortranNode), writeConsiderationEnd)

			varWritesInSrc = map (\var -> snd (getAccessLocationsInsideSrcSpan varAccessAnalysis var writeConsiderationSrc)) initWrites
			varWritesInSrcExcludingKernelRange = map (filter (\x -> not (srcSpanInSrcSpanRange kernelStartSrc kernelEndSrc x))) varWritesInSrc

			bufferWritePositions = zip initWrites (map (\srcs -> fromMaybe (nullSrcSpan) (getLatestSrcSpan srcs)) varWritesInSrcExcludingKernelRange)
			bufferWritePositionsExcludingNull = filter (\(var, src) -> src /= nullSrcSpan) bufferWritePositions
			bufferWritePositionsKernelStart = map (\(var, src) -> if checkSrcLocBefore (fst kernelStartSrc) (fst src) then (var, kernelStartSrc) else (var, src)) bufferWritePositionsExcludingNull

			bufferWritesBeforeSrc = filter (\(var, src) -> src == kernelStartSrc) bufferWritePositionsExcludingNull 
			bufferWritesAfterSrc = filter (\(var, src) -> src /= kernelStartSrc) bufferWritePositionsExcludingNull 

--	A similar approach to the function above, except we are looking for locations where a variable is read rather than written to. The consideration range
--	is after the kernel calls (including any loops that the kernel calls appear in) and any read that happens before the kernel calls in a loop mean that a
--	buffer read MUST happen directly after the kernel calls.

--	(Following functions are used to consider for loops when placing buffer reads and writes in the main)
generateBufferReadPositions :: [VarName Anno] -> Program Anno -> SrcSpan -> SrcSpan -> VarAccessAnalysis -> ([(VarName Anno, SrcSpan)], [(VarName Anno, SrcSpan)])
generateBufferReadPositions finalReads mainAst kernelStartSrc kernelEndSrc varAccessAnalysis = (bufferReadsBeforeSrc, bufferReadsAfterSrc)
		where
			fortranNode = (extractFirstFortran mainAst)
			readConsiderationStart = findReadConsiderationStart (snd kernelEndSrc) fortranNode
			readConsiderationSrc = (readConsiderationStart, snd (srcSpan fortranNode))

			varReadsInSrc = map (\var -> snd (getAccessLocationsInsideSrcSpan varAccessAnalysis var readConsiderationSrc)) finalReads
			varReadsInSrcExcludingKernelRange = map (filter (\x -> not (srcSpanInSrcSpanRange kernelStartSrc kernelEndSrc x))) varReadsInSrc

			bufferReadPositions = zip finalReads (map (\srcs -> fromMaybe (nullSrcSpan) (getEarliestSrcSpan srcs)) varReadsInSrcExcludingKernelRange)
			bufferReadPositionsExcludingNull = filter (\(var, src) -> src /= nullSrcSpan) bufferReadPositions
			bufferReadPositionsKernelStart = map (\(var, src) -> if checkSrcLocBefore (fst src) (fst kernelEndSrc) then (var, kernelEndSrc) else (var, src)) bufferReadPositionsExcludingNull

			bufferReadsBeforeSrc = filter (\(var, src) -> src == kernelEndSrc) bufferReadPositionsExcludingNull
			bufferReadsAfterSrc = filter (\(var, src) -> src /= kernelEndSrc) bufferReadPositionsExcludingNull

--	Traverse the ast looking for a target location. If the target location is found to be within a loop, return the location of the end of the loop,
--	otherwise return the first location that appears at the target location.
findWriteConsiderationEnd :: SrcLoc -> Fortran Anno ->  SrcLoc
findWriteConsiderationEnd targetSrcLoc (For  _ src _ _ _ _ fortran) = findWriteConsiderationEnd (snd src) fortran
findWriteConsiderationEnd targetSrcLoc codeSeg 	|	reachedTarget = (snd src)
												|	otherwise = fromMaybe (nullSrcLoc) (getEarliestSrcLoc recursiveResult)
		where
			src = srcSpan codeSeg
			reachedTarget = checkSrcLocEqualLines targetSrcLoc (snd src)

			recursiveResult = filter (/= nullSrcLoc) (gmapQ (mkQ nullSrcLoc (findWriteConsiderationEnd targetSrcLoc)) codeSeg)

--	Traverse the ast looking for a target location. If the target location is found to be within a loop, return the location of the start of the loop,
--	otherwise return the first location that appears at the target location.
findReadConsiderationStart :: SrcLoc -> Fortran Anno -> SrcLoc
findReadConsiderationStart targetSrcLoc (For _ src _ _ _ _ fortran) 	|	childResult /= nullSrcLoc = fst src
																		|	otherwise = nullSrcLoc
		where
			childResult = findReadConsiderationStart targetSrcLoc fortran
findReadConsiderationStart targetSrcLoc codeSeg 	|	reachedTarget = fst src
													|	otherwise = fromMaybe nullSrcLoc (getEarliestSrcLoc recursiveResult)
		where
			src = srcSpan codeSeg
			reachedTarget = checkSrcLocEqualLines targetSrcLoc (snd src)

			recursiveResult = filter (/= nullSrcLoc) (gmapQ (mkQ nullSrcLoc (findReadConsiderationStart targetSrcLoc)) codeSeg)		


replaceSubroutineAppearences :: SubroutineTable -> [Program Anno] -> [Program Anno]
replaceSubroutineAppearences subTable [] = []
replaceSubroutineAppearences subTable (firstProgram:programs) = updated:replaceSubroutineAppearences subTable programs
		where 
			updated = everywhere (mkT (replaceSubroutine subTable)) firstProgram

replaceSubroutine :: SubroutineTable -> ProgUnit Anno -> ProgUnit Anno
replaceSubroutine subTable codeSeg = case codeSeg of
								(Sub _ _ _ (SubName _ str) _ _) -> subroutineTable_ast (DMap.findWithDefault (codeSeg, "") str subTable)
								_ -> codeSeg

--	This function has no type signiture to allow it to be appled to any AST node, making using of SYB generics.
flattenSubroutineAppearences subTable argTransTable mainAst = updated
		where
			subroutines = DMap.keys subTable
			updated = everywhere (mkT (flattenSubroutineCall_container subTable argTransTable)) mainAst

--	This function only makes changes to AST nodes that contain 'Call' nodes. It must consider the level above the call because the source
--	line information of surrounding nodes must be updated if a new subroutine body is folded into the original AST. 
--	STEPS:
--		-	Determine whether a node contains a call node, if so, continue, otherwise return the original node.
--		-	Extract the name of the called subroutine and retrieve the appropriate source line info of the body from the subroutine table.
--		-	Retrieve the appropriate argument translation entry for this subroutine, so that any arguments/variables that must be changed
--			in the body of the subroutine can be updated.
--		-	Call 'flattenSubroutineCall'
--			+	Retrieve the subroutine body
--			+	Adjust line/location information in the retreived body code so that it fits into the new (usually the main) AST.
--			+	Return the subroutine body
--		-	Return the subroutine body
flattenSubroutineCall_container :: SubroutineTable -> ArgumentTranslationSubroutines -> Fortran Anno -> Fortran Anno
flattenSubroutineCall_container subTable argTransTable containerSeg 	| 	containedCalls /= [] = containerWithFlattenedSub
																		|	otherwise = containerSeg														
		where
			containedCalls = foldl (++) [] (gmapQ (mkQ [] extractCalls) containerSeg)
			(Call _ _ callExpr _) = if (length containedCalls > 1) then error "flattenSubroutineCall_container: multiple contained calls unsupported" else head containedCalls

			subroutineSrc = case DMap.lookup subroutineName subTable of
										Nothing -> nullSrcSpan
										Just subroutine -> srcSpan (subroutineTable_ast subroutine)
			subroutineLineSize = srcSpanLineCount subroutineSrc
			containerWithScrShifts = gmapT (mkT (ignoreCall_T (shiftSrcSpanLineGlobal subroutineLineSize))) containerSeg

			subroutineName = if extractVarNames callExpr == [] then (error "flattenSubroutineCall: callExpr\n" ++ (show callExpr))  else varNameStr (head (extractVarNames callExpr))
			subroutineArgTrans = DMap.findWithDefault (error "flattenSubroutineCall_container: subroutineArgTrans") subroutineName argTransTable
			containerWithFlattenedSub = gmapT (mkT (flattenSubroutineCall subTable subroutineArgTrans)) containerWithScrShifts

flattenSubroutineCall :: SubroutineTable -> ArgumentTranslation -> Fortran Anno -> Fortran Anno
flattenSubroutineCall subTable argTransTable (Call anno cSrc callExpr args) = fromMaybe callFortran shiftedSubroutineReplacement
		where
			callFortran = (Call anno cSrc callExpr args)
			subroutineName = if extractVarNames callExpr == [] then (error "flattenSubroutineCall:callExpr\n" ++ (show callExpr))  else varNameStr (head (extractVarNames callExpr))
			subroutineReplacement = case DMap.lookup subroutineName subTable of
										Nothing -> Nothing
										Just subroutine -> Just (substituteArguments argTransTable callFortran (subroutineTable_ast subroutine))

			subroutineSrc = srcSpan (fromMaybe (error "flattenSubroutineCall: subroutineSrc") subroutineReplacement)
			((SrcLoc _ callLineStart _), _) =  cSrc
			((SrcLoc _ bodyLineStart _), _) =  subroutineSrc
			bodyOffset = (callLineStart -  bodyLineStart) + 1

			shiftedSubroutineReplacement = case subroutineReplacement of
												Nothing -> Nothing
												Just rep -> Just (shiftSrcSpanLineGlobal bodyOffset rep) 
flattenSubroutineCall subTable argTransTable codeSeg = codeSeg

--	Function is called by main to compile the set of argument translation tables for all of the parsed subroutines.
--	STEPS:
--		-	Extract all calls to subroutines in the chosen AST (usually the main)
--		-	Call 'generateArgumentTranslationSubroutines' on each call:
--			+	Extract the appropriate subroutine from the subroutine table, if it exists
--			+	Extract args from the call and the subroutine definition
--			+	Match pairs of args from the call and definition according to the order in which they appear, adding
--				each pair to a Data.Map.
--			+	Return the Data.Map
--		-	Stored each Data.Map in another Data.Map, whose keys are subroutine names.
extractArgumentTranslationSubroutines subTable ast = argTransTable
		where
			subroutines = DMap.keys subTable
			calls = everything (++) (mkQ [] extractCalls) ast
			argTransTable = foldl (generateArgumentTranslationSubroutines subTable) (DMap.empty) calls

generateArgumentTranslationSubroutines :: SubroutineTable -> ArgumentTranslationSubroutines -> Fortran Anno -> ArgumentTranslationSubroutines
generateArgumentTranslationSubroutines subTable argTable (Call anno src callExpr arglist) = DMap.insert subroutineName varNameReplacements argTable
		where
			varNameReplacements = generateArgumentTranslation subTable (Call anno src callExpr arglist)
			subroutineName = varNameStr (head (extractVarNames callExpr))

generateArgumentTranslation :: SubroutineTable -> Fortran Anno -> ArgumentTranslation
generateArgumentTranslation subTable (Call anno src callExpr arglist) = varNameReplacements
		where
			subroutineName = varNameStr (head (extractVarNames callExpr))
			subroutineMaybe = DMap.lookup subroutineName subTable
			(subroutineParsed, subroutine) = case subroutineMaybe of
									Nothing -> (False, error "generateArgumentTranslationSubroutines")
									Just sub -> (True, sub)
			(Sub _ _ _ _ arg _) = subroutineTable_ast subroutine

			callArgs = everything (++) (mkQ [] extractExpr_list) arglist
			bodyArgs = everything (++) (mkQ [] extractArgName) arg

			callArgs_varNames = map (\x -> if extractVarNames x == [] then error ("substituteArguments: " ++ (show x)) else  head (extractVarNames x)) callArgs
			bodyArgs_varNames = map (\(ArgName _ str) -> VarName nullAnno str) bodyArgs

			varNameReplacements = foldl (\dmap (old, new) -> DMap.insert old new dmap) DMap.empty (zip bodyArgs_varNames callArgs_varNames)

--	The following 5 functions allow for the use of the argument translation datastructures. These functions actually produce the translated
--	versions of VarNames.
translateArgumentsSubroutine :: ArgumentTranslationSubroutines -> String -> [VarName Anno] -> [VarName Anno]
translateArgumentsSubroutine argTranslations subroutineName args = map (translateArgument argTranslationsSubroutine) args
		where
			argTranslationsSubroutine = getSubroutineArgumentTranslation argTranslations subroutineName

translateArgumentSubroutine :: ArgumentTranslationSubroutines -> String -> VarName Anno -> VarName Anno
translateArgumentSubroutine argTranslations subroutineName arg = translateArgument argTranslationsSubroutine arg
		where
			argTranslationsSubroutine = getSubroutineArgumentTranslation argTranslations subroutineName

getSubroutineArgumentTranslation :: ArgumentTranslationSubroutines -> String -> ArgumentTranslation
getSubroutineArgumentTranslation argTranslation subName = DMap.findWithDefault (DMap.empty) subName argTranslation

translateArguments :: ArgumentTranslation -> [VarName Anno] -> [VarName Anno]
translateArguments argTranslations args = map (translateArgument argTranslations) args

translateArgument :: ArgumentTranslation -> VarName Anno -> VarName Anno
translateArgument argTranslations var = DMap.findWithDefault (var) var argTranslations

--	The following four functions deal with placing buffer reads into the AST of the main. These reads happen when a value has been calculated
--	on the device but is then used by the host. Given a point in the main AST where the OpenCL work is done, the AST and a list of variables
--	that exist on the device at the end of its use, the following happens.
--	STEPS
--		-	Find Assignments after the provided SrcSpan that read one of the vars on the device.
--		-	Insert an OpenCLBufferRead before the assignment
insertBufferReads mainAst varsOnDevice openCLendSrc = everywhere (mkT (insertBufferReads_block varsOnDevice openCLendSrc)) mainAst

insertBufferReads_block :: [VarName Anno] -> SrcSpan -> Block Anno -> Block Anno
insertBufferReads_block varsOnDevice afterSrc inputBlock = gmapT (mkT (insertBufferReads_fortran varsOnDevice afterSrc)) inputBlock

insertBufferReads_fortran :: [VarName Anno] -> SrcSpan -> Fortran Anno -> Fortran Anno
insertBufferReads_fortran varsOnDevice afterSrc (FSeq fseqAnno fseqSrc (Assg assgAnno assgSrc expr1 expr2) fortran2) 	| 	rightLocation = gmapT (mkT (insertBufferReads_fortran newVarsOnDevice afterSrc)) bufferReadFortran
																														|	otherwise = 	gmapT (mkT (insertBufferReads_fortran varsOnDevice afterSrc)) codeSeg
		where
			codeSeg = (FSeq fseqAnno fseqSrc (Assg assgAnno assgSrc expr1 expr2) fortran2)
			rightLocation = checkSrcSpanBefore afterSrc assgSrc 
			readVarNames = listRemoveDuplications ((extractAllVarNames expr2) ++ (foldl (\accum item -> accum ++ extractVarNames item) [] (extractContainedVars expr1)))
			readVarNamesOnDevice = listIntersection varsOnDevice readVarNames

			bufferReadFortran = buildBufferReadFortran readVarNamesOnDevice codeSeg

			newVarsOnDevice = listSubtract varsOnDevice readVarNamesOnDevice
insertBufferReads_fortran varsOnDevice afterSrc codeSeg 	| 	rightLocation = gmapT (mkT (insertBufferReads_fortran newVarsOnDevice afterSrc)) bufferReadFortran
															|	otherwise = 	gmapT (mkT (insertBufferReads_fortran varsOnDevice afterSrc)) codeSeg
		where
			exprs = foldl (++) [] (gmapQ (mkQ [] extractExpr_list) codeSeg)
			rightLocation = checkSrcSpanBefore afterSrc (srcSpan codeSeg)
			readVarNames = listRemoveDuplications (extractAllVarNames codeSeg)
			readVarNamesOnDevice = listIntersection varsOnDevice readVarNames

			bufferReadFortran = buildBufferReadFortran readVarNamesOnDevice codeSeg
			newVarsOnDevice = listSubtract varsOnDevice readVarNamesOnDevice

insertBufferReadsAfter varSrcPairs ast = everywhere (mkT (insertFortranAfter_block fortranSrcPairs)) ast
		where 
			fortranSrcPairs = map (\(var, src) -> (OpenCLBufferRead nullAnno src var, src)) varSrcPairs

insertBufferReadsBefore varSrcPairs ast = everywhere (mkT (insertFortranBefore_block fortranSrcPairs)) ast
		where 
			fortranSrcPairs = map (\(var, src) -> (OpenCLBufferRead nullAnno src var, src)) varSrcPairs


insertBufferWritesAfter varSrcPairs ast = everywhere (mkT (insertFortranAfter_block fortranSrcPairs)) ast
		where 
			fortranSrcPairs = map (\(var, src) -> (OpenCLBufferWrite nullAnno src var, src)) varSrcPairs

insertBufferWritesBefore varSrcPairs ast = everywhere (mkT (insertFortranBefore_block fortranSrcPairs)) ast
		where 
			fortranSrcPairs = map (\(var, src) -> (OpenCLBufferWrite nullAnno src var, src)) varSrcPairs

insertFortranAfter_block :: [(Fortran Anno, SrcSpan)] -> Block Anno -> Block Anno
insertFortranAfter_block fortranSrcPairs (Block anno useBlock imp src decl fortran) = Block anno useBlock imp src decl (insertFortranAfter_fortran fortranSrcPairs fortran)

insertFortranBefore_block :: [(Fortran Anno, SrcSpan)] -> Block Anno -> Block Anno
insertFortranBefore_block fortranSrcPairs (Block anno useBlock imp src decl fortran) = Block anno useBlock imp src decl (insertFortranBefore_fortran fortranSrcPairs fortran)

insertFortranAfter_fortran :: [(Fortran Anno, SrcSpan)] -> Fortran Anno -> Fortran Anno
insertFortranAfter_fortran fortranSrcPairs ast = foldl (\astAccum (fort, src) -> insertAfterSrc fort src astAccum) ast fortranSrcPairs

insertFortranBefore_fortran :: [(Fortran Anno, SrcSpan)] -> Fortran Anno -> Fortran Anno
insertFortranBefore_fortran fortranSrcPairs ast = foldl (\astAccum (fort, src) -> insertBeforeSrc fort src astAccum) ast fortranSrcPairs

insertAfterSrc :: Fortran Anno -> SrcSpan -> Fortran Anno -> Fortran Anno
insertAfterSrc newFortran src ast = insertFortranAfterSrc newFortran src ast

insertBeforeSrc :: Fortran Anno -> SrcSpan -> Fortran Anno -> Fortran Anno
insertBeforeSrc newFortran src ast = insertFortranBeforeSrc newFortran src ast

insertFortranAfterSrc :: Fortran Anno -> SrcSpan -> Fortran Anno -> Fortran Anno
insertFortranAfterSrc newFortran src (FSeq fseqAnno fseqSrc fortran1 fortran2) 	|	correctPosition = (FSeq fseqAnno fseqSrc fortran1 (FSeq nullAnno nullSrcSpan newFortran fortran2))
																				|	otherwise = FSeq fseqAnno fseqSrc updateFortran1 updateFortran2
		where
			((SrcLoc _ startLine _), (SrcLoc _ endLine _)) = src
			((SrcLoc _ f1LineStart _), (SrcLoc _ f1LineEnd _)) = srcSpan fortran1
			correctPosition = src == srcSpan fortran1
			updateFortran1 = insertFortranAfterSrc newFortran src fortran1
			updateFortran2 = if (updateFortran1 == fortran1) then insertFortranAfterSrc newFortran src fortran2 else fortran2
insertFortranAfterSrc newFortran src codeSeg 	|	correctPosition = FSeq nullAnno nullSrcSpan codeSeg newFortran
												|	otherwise = gmapT (mkT (insertFortranAfterSrc newFortran src)) codeSeg
		where
			((SrcLoc _ startLine _), _) = src
			((SrcLoc _ codeSegLineStart _), _) = srcSpan codeSeg
			correctPosition = src == srcSpan codeSeg

insertFortranBeforeSrc :: Fortran Anno -> SrcSpan -> Fortran Anno -> Fortran Anno
insertFortranBeforeSrc newFortran src (FSeq fseqAnno fseqSrc fortran1 fortran2) 	|	correctPosition = FSeq nullAnno nullSrcSpan newFortran ((FSeq fseqAnno fseqSrc fortran1 fortran2)) -- (FSeq fseqAnno fseqSrc fortran1 (FSeq nullAnno nullSrcSpan newFortran fortran2))
																					|	otherwise = FSeq fseqAnno fseqSrc updateFortran1 updateFortran2
		where
			((SrcLoc _ startLine _), (SrcLoc _ endLine _)) = src
			((SrcLoc _ f1LineStart _), (SrcLoc _ f1LineEnd _)) = srcSpan fortran1
			correctPosition = src == srcSpan fortran1
			updateFortran1 = insertFortranBeforeSrc newFortran src fortran1
			updateFortran2 = if (updateFortran1 == fortran1) then insertFortranBeforeSrc newFortran src fortran2 else fortran2
insertFortranBeforeSrc newFortran src codeSeg 	|	correctPosition = FSeq nullAnno nullSrcSpan newFortran codeSeg
												|	otherwise = gmapT (mkT (insertFortranBeforeSrc newFortran src)) codeSeg
		where
			((SrcLoc _ startLine _), _) = src
			((SrcLoc _ codeSegLineStart _), _) = srcSpan codeSeg
			correctPosition = src == srcSpan codeSeg

buildBufferReadFortran :: [VarName Anno] -> Fortran Anno -> Fortran Anno
buildBufferReadFortran [] followingFortran = followingFortran
buildBufferReadFortran (varToRead:[]) followingFortran = FSeq nullAnno nullSrcSpan oclRead followingFortran
		where
			oclRead = (OpenCLBufferRead nullAnno nullSrcSpan varToRead)
buildBufferReadFortran (varToRead:vars) followingFortran = FSeq nullAnno nullSrcSpan oclRead (buildBufferReadFortran vars followingFortran)
		where
			oclRead = (OpenCLBufferRead nullAnno nullSrcSpan varToRead)

buildBufferWriteFortran :: [VarName Anno] -> Fortran Anno -> Fortran Anno
buildBufferWriteFortran [] followingFortran = followingFortran
buildBufferWriteFortran (varToWrite:[]) followingFortran = FSeq nullAnno nullSrcSpan oclWrite followingFortran
		where
			oclWrite = (OpenCLBufferWrite nullAnno nullSrcSpan varToWrite)
buildBufferWriteFortran (varToWrite:vars) followingFortran = FSeq nullAnno nullSrcSpan oclWrite (buildBufferWriteFortran vars followingFortran)
		where
			oclWrite = (OpenCLBufferWrite nullAnno nullSrcSpan varToWrite)

--	The following function determines which variables should be 'initialised' before any OpenCL kernel call. This is done by considering the kernels in order to find kernel arguments that
--	are read by the kernel before any writen occurs on them. As in, the kernels expect that the variable/argument already exists on the device.
extractSubroutineInitBufferWrites :: ProgUnit Anno -> [VarName Anno]
extractSubroutineInitBufferWrites ast = fst (foldl (\(accum_r, accum_w) (item_r, item_w) -> (accum_r ++ (listSubtract item_r accum_w), accum_w ++ item_w)) ([], []) (zip reads writes))
		where
			kernels = extractKernels ast
			reads = map extractKernelReads kernels
			writes = map extractKernelWrites kernels

-- 	The following function determines which variables will exist on the device when all of the OpenCL work is done. Using a similar strategy to the function above, finds the arguments that
--	are written by the kernel without any following kernel reading them afterward. 
extractSubroutineFinalBufferReads :: ProgUnit Anno -> [VarName Anno]
extractSubroutineFinalBufferReads ast = snd (foldl (\(accum_r, accum_w) (item_r, item_w) -> (accum_r ++ item_r, accum_w ++ (listSubtract item_w accum_r))) ([], []) (zip reads writes))
		where
			kernels = extractKernels ast
			reads = reverse $ map extractKernelReads kernels
			writes = reverse $ map extractKernelWrites kernels

generateKernelCallSrcRange :: SubroutineTable -> Program Anno -> (SrcSpan, SrcSpan)
generateKernelCallSrcRange subTable ast = (kernelsStart, kernelsEnd)
		where
			callsAndStrings = everything (++) (mkQ [] extractCallsWithStrings) ast
			callSrcs = map (\(callCode, callString) -> srcSpan callCode) (filter (\(callCode, callString) -> elem callString (DMap.keys subTable)) callsAndStrings)
			kernelsStart = (fromMaybe (error "generateKernelCallSrcRange") (getEarliestSrcSpan callSrcs))
			kernelsEnd = (fromMaybe (error "generateKernelCallSrcRange") (getLatestSrcSpan callSrcs))
			-- kernelsStart = fst (fromMaybe (error "generateKernelCallSrcRange") (getEarliestSrcSpan callSrcs))
			-- kernelsEnd = snd (fromMaybe (error "generateKernelCallSrcRange") (getLatestSrcSpan callSrcs))

--	Taking a subroutine table and a set of kernel pairs (old, new), replace the entries of the 'old' kernels with with their associated 'new' kernels, using the same
--	subroutine names and filenames.
replaceKernels_foldl :: [(Fortran Anno, Fortran Anno)] -> SubroutineTable -> String -> SubroutineTable
replaceKernels_foldl kernelPairs subTable subName = DMap.insert subName (newAst, filename) subTable
		where
			ast = subroutineTable_ast subroutine
			filename = subroutineTable_filename subroutine
			subroutine = DMap.findWithDefault (error "replaceKernels_foldl") subName subTable
			newAst = replaceKernels kernelPairs ast

replaceKernels :: [(Fortran Anno, Fortran Anno)] -> ProgUnit Anno -> ProgUnit Anno
replaceKernels kernelPairs subroutine = foldl (\accumSub (old, optim) -> replaceFortran accumSub old optim) subroutine kernelPairs

optimseBufferTransfers_kernel :: VarAccessAnalysis -> Program Anno -> Program Anno
optimseBufferTransfers_kernel varAccessAnalysis ast = ast_optimisedBetweenKernels
		where
			ast_optimisedBetweenKernels = compareKernelsInOrder varAccessAnalysis kernels ast
			kernels = extractKernels ast
			kernels_optimisedBetween = extractKernels ast_optimisedBetweenKernels

findEarliestInitialisationSrcSpan :: VarAccessAnalysis -> SrcSpan -> [VarName Anno] -> SrcSpan
findEarliestInitialisationSrcSpan varAccessAnalysis kernelsRange initWrites = lastWrite
		where
			writesBefore = foldl (\accum item -> accum ++ snd (getAccessLocationsBeforeSrcSpan varAccessAnalysis item kernelsRange)) [] initWrites
			lastWrite = fromMaybe nullSrcSpan (getLatestSrcSpan writesBefore)

findLatestTearDownSrcSpan :: VarAccessAnalysis -> SrcSpan -> [VarName Anno] -> SrcSpan
findLatestTearDownSrcSpan varAccessAnalysis kernelsRange tearDownReads = firstRead
		where
			readsAfter = foldl (\accum item -> accum ++ fst (getAccessLocationsAfterSrcSpan varAccessAnalysis item kernelsRange)) [] tearDownReads
			kernelsEndLine = (snd kernelsRange, snd kernelsRange)
			firstRead = fromMaybe kernelsEndLine (getEarliestSrcSpan readsAfter)

--	Using the variable access analysis from the flattened main AST, strip away those arguments that the kernel writes back to the host that are never read
--	by the host after the end of the kernel and before the end of ALL of the kernels (As in, there is no need for the variable to be on the host while OpenCL stuff is
--	still happening. Similarily, strip away any arguments that the kernel reads from the host that are not written to by the host between kernels.
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

--	Taking the kernels in the order that they are called by the subroutines, and in the order that the subroutines are called by the host, eliminate pairs of
--	arguments that cancel each other out. For example, kernel A writes back var x and kernel B reads it again soon after, with no host interaction - in this case,
--	both the read and the write of x can be removed from the respective kernels.
compareKernelsInOrder :: VarAccessAnalysis -> [Fortran Anno] -> Program Anno -> Program Anno
compareKernelsInOrder varAccessAnalysis [] ast = ast
compareKernelsInOrder varAccessAnalysis kernels ast = compareKernelsInOrder varAccessAnalysis (newKernels) newAst
		where
			currentKernel = head kernels
			(newFirstKernel, newKernels) = eliminateBufferPairsKernel_recurse varAccessAnalysis currentKernel (tail kernels) []
			newAst = foldl (\accumAst (oldFortran, newFortran) -> replaceFortran accumAst oldFortran newFortran) ast (zip kernels (newFirstKernel:newKernels))

eliminateBufferPairsKernel_recurse :: VarAccessAnalysis -> Fortran Anno -> [Fortran Anno] -> [SrcSpan] -> (Fortran Anno, [Fortran Anno])
eliminateBufferPairsKernel_recurse varAccessAnalysis firstKernel [] ignoredSpans = (firstKernel, [])
eliminateBufferPairsKernel_recurse varAccessAnalysis firstKernel kernels ignoredSpans = (resursiveCall_firstKernel, newSecondKernel:resursiveCall_kernels)
		where
			secondKernel = head kernels
			(newFirstKernel, newSecondKernel) = eliminateBufferPairsKernel varAccessAnalysis ignoredSpans firstKernel secondKernel
			(resursiveCall_firstKernel, resursiveCall_kernels) = eliminateBufferPairsKernel_recurse varAccessAnalysis newFirstKernel (tail kernels) ((srcSpan secondKernel):ignoredSpans)

eliminateBufferPairsKernel :: VarAccessAnalysis -> [SrcSpan] -> Fortran Anno -> Fortran Anno -> (Fortran Anno, Fortran Anno)
eliminateBufferPairsKernel varAccessAnalysis ignoredSpans firstKernel secondKernel = (newFirstKernel, newSecondKernel)
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

			k1src = errorLocationFormatting (srcSpan firstKernel)
			k2src = errorLocationFormatting (srcSpan secondKernel)

			newFirstKernel = replaceKernelWrites firstKernel newFirstBufferReads
			newSecondKernel = replaceKernelReads secondKernel newSecondBufferWrites

constructSubroutineTable :: [(Program Anno, String)] -> SubroutineTable
constructSubroutineTable programs = foldl (\accum (ast, filename) -> DMap.insert (extractProgUnitName ast) (ast, filename) accum) DMap.empty parsedSubroutines
		where
			parsedSubroutines = foldl (\accum (ast, filename) -> accum ++ (map (\x -> (x, filename)) (extractSubroutines ast))) [] programs

--
--	UTILITIES
--

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


ignoreCall_T :: (Fortran Anno -> Fortran Anno) -> Fortran Anno -> Fortran Anno
ignoreCall_T func codeSeg = case codeSeg of
								Call _ _ _ _ -> codeSeg
								_ -> func codeSeg

extractCalls codeSeg = case codeSeg of
							Call _ _ _ _ -> [codeSeg]
							_ -> []

extractCallsWithStrings :: Fortran Anno -> [(Fortran Anno, String)]
extractCallsWithStrings codeSeg = case codeSeg of
							Call _ _ callExpr _ -> [(codeSeg, varNameStr $ head (extractVarNames callExpr))]
							_ -> []

substituteArguments :: ArgumentTranslation -> Fortran Anno -> ProgUnit Anno -> (Fortran Anno)
substituteArguments argTransTable (Call _ _ _ arglist) (Sub _ _ _ _ arg (Block _ _ _ _ _ for)) = everywhere (mkT (replaceArgs argTransTable)) for

replaceArgs :: DMap.Map (VarName Anno) (VarName Anno) -> VarName Anno -> VarName Anno
replaceArgs varNameReplacements varname = DMap.findWithDefault varname varname varNameReplacements

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