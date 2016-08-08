module Main 

where

-- 	This is the top level of the whole compiler. This module makes calls to all of the analysis, code emission and loop fusion elements of the
-- 	the whole process. 

--	High Level Steps
--		1)	Command line arguments are processed
--		2)	The main program and files listed after the '-modules' flag are parsed and held separately
--		3)	The non-main files are processed to separate out individual subroutines and place them in a 'subroutine table'
--		4)	Each one of the subroutines is parallelised using functions located in 'Transformer.hs', which in turn uses 
--			functionality present in 'LoopAnalysis.hs'
--		5)	Each of the subroutines is then subjected to loop/kernel fusion, in an attempt to reduce the number of kernels
--			That will be called.
--		6)	Annotations that describe obstacles to parallelism and information of loop fusion are then extracted and later 
--			presented to the user (if in verbose mode).
--		7)	At this point, two divergent paths are taken using the current subroutine ASTs. 
--			a)	One version of the ASTs goes through buffer transfer optimisation which involves reducing the number of variables 
--				that make up the read and written arguments for a kernel. Reducing these arguments results in less buffer manipulation and 
--				therefore less memory use in the final program. This process (and the final emission process) requires argument translation
-- 				information and so that it generated at this stage too. The argument translation information is Data.Map for each subroutine.
--				where each key is a variable name in the subroutine and the associated item is the name of that variable in the main, when you
--				consider calls to the subroutine. This is used to ensure that global buffers are used correctly.
--			b)	The other version of the ASTs is used to produce a list of (AST, filename) pairs. This list is used during code emission
--				to produce kernel code as it is necessary to know the filenames for each AST to ease the generation process. 
-- 			c) 	The buffer optimised versions of the ASTs also go through this process as the non buffer optimised version as they are needed 
--				with their filenames to generate host code.
--		8)	Finally, code is emitted.

import Data.Generics 			(Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran
import System.Environment
import qualified Data.Map as DMap 

import Transformer 				(paralleliseProgUnit_foldl, combineKernelProgUnit_foldl)
import BufferTransferAnalysis 	(optimiseBufferTransfers, replaceSubroutineAppearences)
import SubroutineTable 			(constructSubroutineTable, extractArgumentTranslationSubroutines)
import LanguageFortranTools 	(Anno, appendToMap, errorLocationFormatting, nullAnno, nullSrcSpan, generateVar, outputTab, parseFile, compilerName)
import CodeEmitter 				(emit)

-- main :: IO [()]
main = do

	putStr "\nConcerns/To do:"
	putStr ("\n" 	++ outputTab ++ "- Improve naming conventions")
	putStr ("\n" 	++ outputTab ++ "- Convert clusters of buffer reads/writes to subroutine calls")
	putStr ("\n" 	++ outputTab ++ "- Consider the possibility of a call to a host subroutine happening after a\n"
					++ outputTab ++ "call to an OpenCL kernel inside a small loop. The vars that are written by the\n"
					++ outputTab ++ "host must therefore be rewritten to buffers at the start of the loop")
	putStr ("\n" 	++ outputTab ++ "- Problem with reducing into array elements. For example, in press when p(i,j,k)\n"
					++ outputTab ++ "is a reduction variable in the sor loop. Problem seems to be worse when loop is\n"
					++ outputTab ++ "detected as an iterative reduction rather than a normal reduction (this loop is\n"
					++ outputTab ++ "a normal reduction when the calls to boundp1 and boundp2 are commented out). The\n"
					++ outputTab ++ "issue may stem from the fact that no initial value for the array element may be\n"
					++ outputTab ++ "produced. It is this problem that means that p is not read back to the host\n
					++ outputTab ++ "for calls to boundp1 and boundp2\n
					)
	putStr "\n\n"

	-- < STEP 1 >
	args <- getArgs
	let argMap = processArgs args
	let filenames = case DMap.lookup filenameFlag argMap of
						Just filenames -> filenames
						Nothing -> usageError
	let mainFilename = case DMap.lookup mainFileFlag argMap of
						Just filenames -> head filenames
						Nothing -> usageError
	let outDirectory = case DMap.lookup outDirectoryFlag argMap of
						Just dirList -> head dirList
						Nothing -> "./"
	let loopFusionBound = case DMap.lookup loopFusionBoundFlag argMap of
						Just bound -> Just (read (head bound) :: Float)
						Nothing -> Nothing
	let verbose = case DMap.lookup verboseFlag argMap of
						Just a -> True
						Nothing -> False
	let fixedForm = case DMap.lookup fiexedFormFlag argMap of
						Just a -> True
						Nothing -> False
	let cppDFlags = DMap.findWithDefault [] cppDefineFlag argMap
	-- </STEP 1 >

	-- < STEP 2 >
	parsedPrograms <- mapM (parseFile cppDFlags fixedForm) filenames
	parsedMain <- parseFile cppDFlags fixedForm mainFilename
	-- </STEP 2 >

	-- < STEP 3 >
	let parsedSubroutines = constructSubroutineTable (zip parsedPrograms filenames)
	let subroutineNames = DMap.keys parsedSubroutines
	let subroutineList = foldl (\accum item -> accum ++ [[DMap.findWithDefault (error "main:subroutineList") item parsedSubroutines]]) [] (subroutineNames)
	-- </STEP 3 >

	
	let (parallelisedSubroutines, parAnnotations) = foldl (paralleliseProgUnit_foldl parsedSubroutines) (DMap.empty, []) subroutineNames 				-- < STEP 4 >	
	let (combinedKernelSubroutines, combAnnotations) = foldl (combineKernelProgUnit_foldl loopFusionBound) (parallelisedSubroutines, []) subroutineNames-- < STEP 5 >
	let annotationListings = map (combineAnnotationListings_map parAnnotations) combAnnotations 														-- < STEP 6 >

	--	< STEP 7a >
	let argTranslations = extractArgumentTranslationSubroutines combinedKernelSubroutines parsedMain
	let (optimisedBufferTransfersSubroutines, newMainAst) = optimiseBufferTransfers combinedKernelSubroutines argTranslations parsedMain 
	--	</STEP 7a >
	
	--	< STEP 7b >
	let parallelisedSubroutineList = map (\x -> DMap.findWithDefault (error "parallelisedSubroutineList") x combinedKernelSubroutines) subroutineNames
	let fileCoordinated_parallelisedMap = foldl (\dmap (ast, filename) -> appendToMap filename ast dmap) DMap.empty parallelisedSubroutineList
	let fileCoordinated_parallelisedList = map (\x -> (DMap.findWithDefault (error "fileCoordinated_parallelisedMap") x fileCoordinated_parallelisedMap, x)) filenames
	--	</STEP 7b >
	
	--	< STEP 7c >
	let fileCoordinated_bufferOptimisedPrograms = zip (replaceSubroutineAppearences optimisedBufferTransfersSubroutines parsedPrograms) filenames
	--	</STEP 7c >
	
	mapM (\(filename, par_anno, comb_anno) -> putStr $ compilerName ++ ": Analysing " ++ filename ++ (if verbose then "\n\n" ++ par_anno ++ "\n" ++ comb_anno ++ "\n" else "\n")) annotationListings

	putStr (compilerName ++ ": Synthesising OpenCL files\n")
	emit outDirectory cppDFlags fixedForm fileCoordinated_parallelisedList fileCoordinated_bufferOptimisedPrograms argTranslations (newMainAst, mainFilename) [] [] -- < STEP 8 >

filenameFlag = "-modules"
outDirectoryFlag = "-out"
loopFusionBoundFlag = "-lfb"
verboseFlag = "-v"
mainFileFlag = "-main"
cppDefineFlag = "-D"
fiexedFormFlag = "-ffixed-form"

flags = [filenameFlag, outDirectoryFlag, loopFusionBoundFlag, cppDefineFlag, verboseFlag, mainFileFlag, fiexedFormFlag]

processArgs :: [String] -> DMap.Map String [String]
processArgs [] = usageError
processArgs (flag:arg:args)	|	elem flag flags = gatherFlag flag (arg:args) []
							|	otherwise 		= gatherFlag filenameFlag (flag:arg:args) []

processArgs' :: [String] -> DMap.Map String [String]
processArgs' (flag:arg:args) 	|	elem flag flags = gatherFlag flag (arg:args) []
								|	otherwise = error (flag ++ " not a recognised argument")
processArgs' (flag:arg)			=	gatherFlag flag arg []
processArgs' [] = DMap.empty

gatherFlag :: String -> [String] -> [String] -> DMap.Map String [String]
gatherFlag flag (arg:args)  collected	|	elem arg flags = DMap.insert flag collected (processArgs' (arg:args))
										|	otherwise = gatherFlag flag args (collected ++ [arg])
gatherFlag flag [] collected = DMap.insert flag collected (processArgs' [])

addArg :: DMap.Map String [String] -> String -> String -> DMap.Map String [String]
addArg argMap flag value = DMap.insert flag newValues argMap
		where
			oldValues = DMap.findWithDefault [] flag argMap
			newValues = oldValues ++ [value]

usageError = error "USAGE: [<filename>] -main <filename> [<flag> <value>]"

combineAnnotationListings_map :: [(String, String)] -> (String, String) -> (String, String, String)
combineAnnotationListings_map annoations (currentFilename, currentAnno) = foldl (\accum (filename, anno) -> if filename == currentFilename then (filename, currentAnno, anno) else accum) (currentFilename, currentAnno, "") annoations