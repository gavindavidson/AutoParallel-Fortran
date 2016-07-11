module Main where

-- 	This is the top level of the whole compiler. This module makes calls to all of the analysis, code emission and loop fusion elements of the
-- 	the whole process. 

--	STEPS
--	-	Process command line arguments
--	-	Parse input files
--	-	Perform constant folding
--	-	Identify parallelism and transform ASTs
--	-	Combine kernels in the ASTs
--	-	Output parallelisation errors and information about kernel fusion
--	-	Emit final code listings.

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran
import System.Environment
import qualified Data.Map as DMap 

import Transformer (paralleliseProgUnit_foldl, combineKernelProgUnit_foldl)
import BufferTransferAnalysis (extractArgumentTranslationSubroutines, optimiseBufferTransfers, replaceSubroutineAppearences, insertBufferReads, constructSubroutineTable)
import LanguageFortranTools (appendToMap, errorLocationFormatting, nullAnno, nullSrcSpan, generateVar, outputTab, parseFile, compilerName, Anno)
import CodeEmitter (emit, initSubroutineName)

-- main :: IO [()]
main = do

	putStr "\nConcerns:"
	-- putStr ("\n" 	++ outputTab ++ "- Consider called subroutines when parallelising.")
	-- putStr ("\n" 	++ outputTab ++ "- Initialisation location must consider loops, not just accesses before\n" 
	-- 				++ outputTab ++ "position")
	putStr ("\n" 	++ outputTab ++ "- Improve naming conventions")
	putStr ("\n" 	++ outputTab ++ "- SELECT CASE statements are no longer supported? Seems a rollback has\n" 
					++ outputTab ++ "happened sometime")
	putStr ("\n" 	++ outputTab ++ "- Document high level steps including function names")
	-- putStr ("\n" 	++ outputTab ++ "- Optimise reads so that only variables that are read later in the program\n" 
	-- 				++ outputTab ++ "are read back from buffers.")
	-- putStr ("\n" 	++ outputTab ++ "- Individual reads, not a subroutine")
	-- putStr ("\n" 	++ outputTab ++ "- Implement \"fixed form\" check after 70 characters if flag in arguments\n"
	-- 				++ outputTab ++ "(Input can now be fixed form, output functionality almost there)")
	-- putStr ("\n" 	++ outputTab ++ "- Do not parallelise calls in loops, rather examine their variable use")
	-- putStr ("\n" 	++ outputTab ++ "- Translate buffer numbers between subroutines. As in, the same buffer being\n"
	-- 				++ outputTab ++ "represented by many variable names across subroutines")
	putStr ("\n" 	++ outputTab ++ "- Consider the possibility of a call to a host subroutine happening after a\n"
					++ outputTab ++ "call to an OpenCL kernel inside a small loop. The vars that are written by the\n"
					++ outputTab ++ "host must therefore be rewritten to buffers at the start of the loop")
	-- putStr ("\n" ++ outputTab ++ "Think we're okay atm..")
	putStr "\n\n"

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

	-- parsedPrograms <- mapM (parseFile cppDFlags False) filenames
	parsedPrograms <- mapM (parseFile cppDFlags fixedForm) filenames

	-- parsedMain <- parseFile cppDFlags False mainFilename
	parsedMain <- parseFile cppDFlags fixedForm mainFilename
	let parsedSubroutines = constructSubroutineTable (zip parsedPrograms filenames)

	-- putStr "\n\nMAIN AST\n\n"
	-- putStr (show parsedMain)

	let subroutineNames = DMap.keys parsedSubroutines
	let subroutineList = foldl (\accum item -> accum ++ [[DMap.findWithDefault (error "main:subroutineList") item parsedSubroutines]]) [] (subroutineNames)

	let (parallelisedSubroutines, parAnnotations) = foldl (paralleliseProgUnit_foldl parsedSubroutines) (DMap.empty, []) subroutineNames
	let (combinedKernelSubroutines, combAnnotations) = foldl (combineKernelProgUnit_foldl loopFusionBound) (parallelisedSubroutines, []) subroutineNames
	let annotationListings = map (combineAnnotationListings_map parAnnotations) combAnnotations

	let parallelisedSubroutineList = map (\x -> DMap.findWithDefault (error "parallelisedSubroutineList") x combinedKernelSubroutines) subroutineNames
	let fileCoordinated_parallelisedMap = foldl (\dmap (ast, filename) -> appendToMap filename ast dmap) DMap.empty parallelisedSubroutineList
	let fileCoordinated_parallelisedList = map (\x -> (DMap.findWithDefault (error "fileCoordinated_parallelisedMap") x fileCoordinated_parallelisedMap, x)) filenames

	let argTranslations = extractArgumentTranslationSubroutines combinedKernelSubroutines parsedMain
	let (optimisedBufferTransfersSubroutines, newMainAst) = optimiseBufferTransfers combinedKernelSubroutines argTranslations parsedMain 
	-- let (optimisedBufferTransfersSubroutines, initTearDownInfo) = optimiseBufferTransfers combinedKernelSubroutines argTranslations parsedMain 
	-- let optimisedBufferTransfersSubroutineList = map (\x -> DMap.findWithDefault (error "optimisedBufferTransfersSubroutineList") x optimisedBufferTransfersSubroutines) subroutineNames
	-- let fileCoordinated_optimisedBufferMap = foldl (\dmap (ast, filename) -> appendToMap filename ast dmap) DMap.empty optimisedBufferTransfersSubroutineList
	-- let fileCoordinated_optimisedBufferList = map (\x -> (DMap.findWithDefault (error "fileCoordinated_optimisedBufferList") x fileCoordinated_optimisedBufferMap, x)) filenames

	let fileCoordinated_bufferOptimisedPrograms = zip (replaceSubroutineAppearences combinedKernelSubroutines parsedPrograms) filenames
	-- let fileCoordinated_bufferOptimisedPrograms = zip (replaceSubroutineAppearences optimisedBufferTransfersSubroutines parsedPrograms) filenames
	-- let ((initWrites, initSrc), (tearDownReads, tearDownSrc)) = initTearDownInfo
	-- let initArgList = generateArgList initWrites
	-- let tearDownArgList = generateArgList tearDownReads
	
	-- let mainAstInit = insertCallAtSrcSpan parsedMain initSrc initSubroutineName initArgList
	-- let newMainAst = insertBufferReads mainAstInit tearDownReads tearDownSrc

	-- let newMainAst = insertCallAtSrcSpan mainAstInit tearDownSrc tearDownSubroutineName tearDownArgList

	-- insertBufferReads
	
	mapM (\(filename, par_anno, comb_anno) -> putStr $ compilerName ++ ": Analysing " ++ filename ++ (if verbose then "\n\n" ++ par_anno ++ "\n" ++ comb_anno ++ "\n" else "\n")) annotationListings
	-- putStr ("tearDownSrc: " ++ errorLocationFormatting tearDownSrc)

	-- putStr "\n\nPARSED SUBROUTINES\n\n"
	-- putStr (show parsedSubroutines)
	-- putStr "\n\nPARALLEL SUBROUTINES\n\n"
	-- putStr (show parallelisedSubroutines)
	-- putStr "\n\nCOMBINED SUBROUTINES\n\n"
	-- putStr (show combinedKernelSubroutines)
	-- putStr "\n\nOPTIMISED SUBROUTINES\n\n"
	-- putStr (show optimisedBufferTransfersSubroutines)
	-- putStr "\n\nNEWMAINAST\n\n"
	-- putStr (show newMainAst)
	-- putStr "\n\nMAIN AST\n\n"
	-- putStr (show parsedMain)
	-- mapM (\(ast, filename) -> putStr ("FILENAME: " ++ filename ++ "\n\n" ++ (show ast))) fileCoordinated_bufferOptimisedPrograms

	putStr (compilerName ++ ": Synthesising OpenCL files\n")
	emit outDirectory cppDFlags fixedForm fileCoordinated_parallelisedList fileCoordinated_bufferOptimisedPrograms argTranslations (newMainAst, mainFilename) [] []

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

-- insertCallAtSrcSpan :: ProgUnit Anno -> SrcSpan -> String -> ArgList Anno -> ProgUnit Anno
insertCallAtSrcSpan ast src callName args = everywhere (mkT (insertCallAtSrcSpan_transformation src callName args)) ast

insertCallAtSrcSpan_transformation ::  SrcSpan -> String -> ArgList Anno -> Fortran Anno -> Fortran Anno
insertCallAtSrcSpan_transformation targetSrc callName args (FSeq anno src fortran1 fortran2) 	|	sameLine || betweenLines = newFSeq
																								|	otherwise = (FSeq anno src fortran1 fortran2)
		where
			newFSeq = FSeq anno src fortran1 (FSeq nullAnno nullSrcSpan newCall fortran2)
			newCall = Call nullAnno targetSrc (generateVar (VarName nullAnno callName)) args

			((SrcLoc _ targetLineNumber _), _) = targetSrc
			((SrcLoc _ fortran1LineNumber _), _) = srcSpan fortran1
			((SrcLoc _ fortran2LineNumber _), _) = srcSpan fortran2

			sameLine = targetLineNumber == fortran1LineNumber
			betweenLines = targetLineNumber > fortran1LineNumber && targetLineNumber < fortran2LineNumber 
insertCallAtSrcSpan_transformation _ _ _ codeSeg = codeSeg


generateArgList :: [VarName Anno] -> ArgList Anno
generateArgList [] = ArgList nullAnno (NullExpr nullAnno nullSrcSpan)
generateArgList vars = ArgList nullAnno (generateESeq vars)

generateESeq :: [VarName Anno] -> Expr Anno
generateESeq (var:[]) = generateVar var
generateESeq (var:vars) = ESeq nullAnno nullSrcSpan (generateESeq vars) (generateVar var)