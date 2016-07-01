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

import Control.Monad
import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List
import System.Environment
import System.Process
import System.Directory
import qualified Data.Map as DMap 

import BufferTransferAnalysis
import CombineKernels
import VarAccessAnalysis
import VarDependencyAnalysis
import LanguageFortranTools
import CodeEmitter
import ConstantFolding
import LoopAnalysis

-- main :: IO [()]
main = do

	putStr "\nConcerns:"
	putStr ("\n" 	++ outputTab ++ "- Consider called subroutines when parallelising.")
	putStr ("\n" 	++ outputTab ++ "- Initialisation location must consider loops, not just accesses before\n" 
					++ outputTab ++ "position")
	putStr ("\n" 	++ outputTab ++ "- Optimise reads so that only variables that are read later in the program\n" 
					++ outputTab ++ "are read back from buffers.")
	putStr ("\n" 	++ outputTab ++ "- Individual reads, not a subroutine")
	putStr ("\n" 	++ outputTab ++ "- Implement \"fixed form\" check after 70 characters if flag in arguments")
	putStr ("\n" 	++ outputTab ++ "- Do not parallelise calls in loops, rather examine their variable use")
	putStr ("\n" 	++ outputTab ++ "- Translate buffer numbers between subroutines. As in, the same buffer being\n"
					++ outputTab ++ "represented by many variable names across subroutines")


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

	let cppDFlags = DMap.findWithDefault [] cppDefineFlag argMap

	parsedPrograms <- mapM (parseFile cppDFlags) filenames
	parsedProgram <- parseFile cppDFlags (head filenames)

	parsedMain <- parseFile cppDFlags mainFilename
	let parsedSubroutines = constructSubroutineTable (zip parsedPrograms filenames)

	let subroutineNames = DMap.keys parsedSubroutines
	let subroutineList = foldl (\accum item -> accum ++ [[DMap.findWithDefault (error "main:subroutineList") item parsedSubroutines]]) [] (subroutineNames)

	let (parallelisedSubroutines, parAnnotations) = foldl (paralleliseProgUnit_foldl) (parsedSubroutines, []) subroutineNames
	let (combinedKernelSubroutines, combAnnotations) = foldl (combineKernelProgUnit_foldl loopFusionBound) (parallelisedSubroutines, []) subroutineNames
	let annotationListings = map (combineAnnotationListings_map parAnnotations) combAnnotations

	let parallelisedSubroutineList = map (\x -> DMap.findWithDefault (error "parallelisedSubroutineList") x combinedKernelSubroutines) subroutineNames
	let fileCoordinated_parallelisedMap = foldl (\dmap (ast, filename) -> appendToMap filename ast dmap) DMap.empty parallelisedSubroutineList
	let fileCoordinated_parallelisedList = map (\x -> (DMap.findWithDefault (error "fileCoordinated_parallelisedMap") x fileCoordinated_parallelisedMap, x)) filenames

	let (optimisedBufferTransfersSubroutines, initTearDownInfo) = optimiseBufferTransfers combinedKernelSubroutines parsedMain 
	let optimisedBufferTransfersSubroutineList = map (\x -> DMap.findWithDefault (error "optimisedBufferTransfersSubroutineList") x optimisedBufferTransfersSubroutines) subroutineNames
	let fileCoordinated_optimisedBufferMap = foldl (\dmap (ast, filename) -> appendToMap filename ast dmap) DMap.empty optimisedBufferTransfersSubroutineList
	let fileCoordinated_optimisedBufferList = map (\x -> (DMap.findWithDefault (error "fileCoordinated_optimisedBufferList") x fileCoordinated_optimisedBufferMap, x)) filenames

	let fileCoordinated_bufferOptimisedPrograms = zip (replaceSubroutineAppearences optimisedBufferTransfersSubroutines parsedPrograms) filenames
	let ((initWrites, initSrc), (tearDownReads, tearDownSrc)) = initTearDownInfo
	let initArgList = generateArgList initWrites
	-- let tearDownArgList = generateArgList tearDownReads
	
	let mainAstInit = insertCallAtSrcSpan parsedMain initSrc initSubroutineName initArgList
	let newMainAst = insertBufferReads mainAstInit tearDownReads tearDownSrc
	-- let newMainAst = insertCallAtSrcSpan mainAstInit tearDownSrc tearDownSubroutineName tearDownArgList

	-- insertBufferReads
	
	mapM (\(filename, par_anno, comb_anno) -> putStr $ compilerName ++ ": Analysing " ++ filename ++ (if verbose then "\n\n" ++ par_anno ++ "\n" ++ comb_anno ++ "\n" else "\n")) annotationListings

	-- putStr "\n\nPARSED SUBROUTINES\n\n"
	-- putStr (show parsedSubroutines)
	-- putStr "\n\nPARALLEL SUBROUTINES\n\n"
	-- putStr (show parallelisedSubroutines)
	-- putStr "\n\nCOMBINED SUBROUTINES\n\n"
	-- putStr (show combinedKernelSubroutines)
	-- putStr "\n\nNEWMAINAST\n\n"
	-- putStr (show newMainAst)
	-- mapM (\(ast, filename) -> putStr ("FILENAME: " ++ filename ++ "\n\n" ++ (show ast))) fileCoordinated_bufferOptimisedPrograms

	putStr (compilerName ++ ": Synthesising OpenCL files\n")
	emit outDirectory cppDFlags fileCoordinated_parallelisedList fileCoordinated_bufferOptimisedPrograms (newMainAst, mainFilename) initWrites tearDownReads

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

paralleliseProgUnit_foldl ::(SubroutineTable, [(String, String)]) -> String -> (SubroutineTable, [(String, String)])
paralleliseProgUnit_foldl (subTable, annoListing) subName = (newSubTable, annoListing ++ [anno])
		where
			(progUnit, filename) = DMap.findWithDefault (error "paralleliseProgUnit_foldl") subName subTable
			(transformedProgUnit, anno) = paralleliseProgUnit progUnit subTable filename 
			newSubTable = DMap.insert subName (transformedProgUnit, filename) subTable

paralleliseProgUnit :: ProgUnit Anno -> SubroutineTable -> String -> (ProgUnit Anno, (String, String))
paralleliseProgUnit progUnit subTable filename = (parallelised, (filename, parAnno))
		where
			foldedConstants = foldConstants progUnit
			parallelised =  paralleliseProgUnit_transform filename subTable (analyseAllVarAccess_progUnit progUnit) foldedConstants
			parAnno = compileAnnotationListing parallelised

combineKernelProgUnit_foldl :: Maybe(Float) -> (SubroutineTable, [(String, String)]) -> String -> (SubroutineTable, [(String, String)])
combineKernelProgUnit_foldl loopFusionBound (subTable, annoListing) subName = (newSubTable, annoListing ++ [anno])
		where
			(progUnit, filename) = DMap.findWithDefault (error "paralleliseProgUnit_foldl") subName subTable
			(transformedProgUnit, anno) = combineKernelProgUnit loopFusionBound progUnit subTable filename 
			newSubTable = DMap.insert subName (transformedProgUnit, filename) subTable

combineKernelProgUnit :: Maybe(Float) -> ProgUnit Anno -> SubroutineTable -> String -> (ProgUnit Anno, (String, String))
combineKernelProgUnit loopFusionBound progUnit subTable filename = (combined, (filename, combAnno))
		where
			combined = combineKernelsProgUnit loopFusionBound (removeAllAnnotations progUnit)
			combAnno = compileAnnotationListing combined

filenameFlag = "-modules"
outDirectoryFlag = "-out"
loopFusionBoundFlag = "-lfb"
verboseFlag = "-v"
mainFileFlag = "-main"
cppDefineFlag = "-D"

flags = [filenameFlag, outDirectoryFlag, loopFusionBoundFlag, cppDefineFlag, verboseFlag, mainFileFlag]

processArgs :: [String] -> DMap.Map String [String]
processArgs [] = usageError
processArgs (arg:args)	|	elem arg flags = usageError
						|	otherwise = gatherFlag filenameFlag (arg:args) []

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

--	The top level function that is called against the original parsed AST
paralleliseProgram :: String -> SubroutineTable -> VarAccessAnalysis -> Program Anno -> Program Anno 
paralleliseProgram filename subTable accessAnalysis codeSeg = map (everywhere (mkT (paralleliseBlock filename subTable (accessAnalysis)))) codeSeg

paralleliseProgUnit_transform :: String -> SubroutineTable -> VarAccessAnalysis -> ProgUnit Anno -> ProgUnit Anno 
paralleliseProgUnit_transform filename subTable accessAnalysis codeSeg = everywhere (mkT (paralleliseBlock filename subTable (accessAnalysis))) codeSeg


--	Called by above to identify actions to take when a Block is encountered. In this case look for loops to parallelise using paralleliseForLoop
paralleliseBlock :: String -> SubroutineTable -> VarAccessAnalysis -> Block Anno -> Block Anno
paralleliseBlock filename subTable accessAnalysis block = gmapT (mkT (paralleliseForLoop filename subTable accessAnalysis)) block

--	When a for loop is encountered this function attempts to parallelise it.
paralleliseForLoop :: String -> SubroutineTable -> VarAccessAnalysis -> Fortran Anno -> Fortran Anno
paralleliseForLoop filename subTable accessAnalysis inp = case inp of
		For _ _ _ _ _ _ _ -> paralleliseLoop filename [] accessAnalysis subTable $ gmapT (mkT (paralleliseForLoop filename subTable accessAnalysis )) inp
		_ -> gmapT (mkT (paralleliseForLoop filename subTable accessAnalysis)) inp

--	Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new parallel (OpenCLMap etc)
--	nodes or the original sub-tree annotated with parallelisation errors. Attempts to map and then to reduce.
paralleliseLoop :: String -> [VarName Anno] -> VarAccessAnalysis -> SubroutineTable -> Fortran Anno -> Fortran Anno
paralleliseLoop filename loopVars accessAnalysis subTable loop = transformedAst		
								where
									newLoopVars = case getLoopVar loop of
										Just a -> loopVars ++ [a]
										Nothing -> loopVars

									nonTempVars = getNonTempVars (srcSpan loop) accessAnalysis
									prexistingVars = getPrexistingVars (srcSpan loop) accessAnalysis
									dependencies = analyseDependencies loop
									flattenedLoop = flattenSubroutineAppearences subTable loop

									-- mapAttempt = paralleliseLoop_map filename flattenedLoop newLoopVars nonTempVars prexistingVars dependencies accessAnalysis
									mapAttempt = paralleliseLoop_map filename loop newLoopVars nonTempVars prexistingVars dependencies accessAnalysis
									mapAttempt_bool = fst mapAttempt
									mapAttempt_ast = snd mapAttempt

									reduceAttempt = paralleliseLoop_reduce filename mapAttempt_ast newLoopVars nonTempVars prexistingVars dependencies accessAnalysis
									reduceAttempt_bool = fst reduceAttempt
									reduceAttempt_ast = snd reduceAttempt

									iterativeReduceAttempt = paralleliseLoop_iterativeReduce filename reduceAttempt_ast Nothing newLoopVars nonTempVars prexistingVars dependencies accessAnalysis
									iterativeReduceAttempt_bool = fst iterativeReduceAttempt
									iterativeReduceAttempt_ast = snd iterativeReduceAttempt

									transformedAst = case mapAttempt_bool of
										True	->  mapAttempt_ast
										False 	-> case reduceAttempt_bool of
													True 	-> reduceAttempt_ast
													False	-> iterativeReduceAttempt_ast

													-- case nextFor_maybe of
													-- 			Nothing -> reduceAttempt_ast
													-- 			Just codeSeg -> iterativeReduceAttempt_ast
									
--	These functions are used to extract a list of varnames that are written to in a particular chunk of code. Used to asses
extractWrites_query :: (Typeable p, Data p) => Fortran p -> [VarName p]
extractWrites_query = everything (++) (mkQ [] extractWrites)

extractWrites :: (Typeable p, Data p) => Fortran p -> [VarName p]
extractWrites (Assg _ _ (Var _ _ list) _) = map (\(varname, exprs) -> varname) list
extractWrites _ = []

--	Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new OpenCLMap nodes or the
--	original sub-tree annotated with reasons why the loop cannot be mapped
paralleliseLoop_map :: String -> Fortran Anno -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarDependencyAnalysis -> VarAccessAnalysis -> (Bool, Fortran Anno)
paralleliseLoop_map filename loop loopVarNames nonTempVars prexistingVars dependencies accessAnalysis
									|	errors_map' == nullAnno 	=	(True, appendAnnotation mapCode (compilerName ++ ": Map at " ++ errorLocationFormatting (srcSpan loop)) "")
									|	otherwise					=	(False, appendAnnotationMap loop errors_map')
									where
										loopWrites = extractWrites_query loop
										loopAnalysis = analyseLoop_map "Cannot map: " [] loopWrites nonTempVars prexistingVars accessAnalysis dependencies loop
										-- loopAnalysis = analyseLoop_map Empty loopVarNames loopWrites nonTempVars accessAnalysis dependencies loop
										errors_map = getErrorAnnotations loopAnalysis
										reads_map = getReads loopAnalysis
										writes_map = getWrites loopAnalysis

										(loopCarriedDeps_bool, evaluated_bool, loopCarriedDeps) = loopCarriedDependencyCheck loop
										-- loopCarriedDeps_bool = loopCarriedDeps /= []
										errors_map' = case loopCarriedDeps_bool of
															True -> case evaluated_bool of
																	True -> DMap.insert (outputTab ++ "Cannot map: Loop carried dependency detected:\n") (formatLoopCarriedDependencies loopCarriedDeps) errors_map
																	False -> DMap.insert (outputTab ++ "Cannot map: Loop carried dependency possible (not evaluated):\n") (formatLoopCarriedDependencies loopCarriedDeps) errors_map
															False -> errors_map

										loopVariables = loopCondtions_query loop

										startVarNames = foldl (\accum (_,x,_,_) -> accum ++ extractVarNames x) [] loopVariables
										endVarNames = foldl (\accum (_,_,x,_) -> accum ++ extractVarNames x) [] loopVariables
										stepVarNames = foldl (\accum (_,_,_,x) -> accum ++ extractVarNames x) [] loopVariables

										varNames_loopVariables = listSubtract (listRemoveDuplications (startVarNames ++ endVarNames ++ stepVarNames)) loopVarNames

										mapCode = OpenCLMap nullAnno (generateSrcSpan filename (srcSpan loop)) 	-- Node to represent the data needed for an OpenCL map kernel
											(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames reads_map)) (map (\(a, _, _, _) -> a) (loopCondtions_query loop)) ++ varNames_loopVariables)	-- List of arguments to kernel that are READ
							 				(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames writes_map)) (map (\(a, _, _, _) -> a) (loopCondtions_query loop))) 	-- List of arguments to kernel that are WRITTEN
											(loopVariables)	-- Loop variables of nested maps
											(removeLoopConstructs_recursive loop) -- Body of kernel code

--	Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new OpenCLReduce nodes or the
--	original sub-tree annotated with reasons why the loop is not a reduction
paralleliseLoop_reduce :: String -> Fortran Anno -> [VarName Anno] -> [VarName Anno]-> [VarName Anno] -> VarDependencyAnalysis -> VarAccessAnalysis -> (Bool, Fortran Anno)
paralleliseLoop_reduce filename loop loopVarNames nonTempVars prexistingVars dependencies accessAnalysis	
									| 	errors_reduce' == nullAnno 	=	(True, appendAnnotation reductionCode (compilerName ++ ": Reduction at " ++ errorLocationFormatting (srcSpan loop)) "")
									|	otherwise					=	(False, appendAnnotationMap loop errors_reduce')
									where
										loopWrites = extractWrites_query loop
										loopAnalysis = analyseLoop_reduce "Cannot reduce: " [] [] loopWrites nonTempVars prexistingVars dependencies accessAnalysis loop 
										-- loopAnalysis = analyseLoop_reduce [] loopVarNames loopWrites nonTempVars dependencies accessAnalysis loop 
										errors_reduce = getErrorAnnotations loopAnalysis
										reductionVariables = getReductionVars loopAnalysis
										reads_reduce = getReads loopAnalysis
										writes_reduce = getWrites loopAnalysis

										(loopCarriedDeps_bool, evaluated_bool, loopCarriedDeps) = loopCarriedDependencyCheck loop

										errors_reduce' = case loopCarriedDeps_bool of
															True -> case evaluated_bool of
																	True -> DMap.insert (outputTab ++ "Cannot reduce: Loop carried dependency detected:\n") (formatLoopCarriedDependencies loopCarriedDeps) errors_reduce
																	False -> DMap.insert (outputTab ++ "Cannot reduce: Loop carried dependency possible (not evaluated):\n") (formatLoopCarriedDependencies loopCarriedDeps) errors_reduce
															False -> errors_reduce


										loopVariables = loopCondtions_query loop

										startVarNames = foldl (\accum (_,x,_,_) -> accum ++ extractVarNames x) [] loopVariables
										endVarNames = foldl (\accum (_,_,x,_) -> accum ++ extractVarNames x) [] loopVariables
										stepVarNames = foldl (\accum (_,_,_,x) -> accum ++ extractVarNames x) [] loopVariables

										varNames_loopVariables = listSubtract (listRemoveDuplications (startVarNames ++ endVarNames ++ stepVarNames)) loopVarNames

										reductionCode = OpenCLReduce nullAnno (generateSrcSpan filename (srcSpan loop))  
 											(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames reads_reduce)) (map (\(a, _, _, _) -> a) (loopCondtions_query loop)) ++ varNames_loopVariables) -- List of arguments to kernel that are READ
							 				(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames writes_reduce)) (map (\(a, _, _, _) -> a) (loopCondtions_query loop))) -- List of arguments to kernel that are WRITTEN
											(loopVariables) -- Loop variables of nested maps
											(listRemoveDuplications (foldl (\accum item -> accum ++ [(item, getValueAtSrcSpan item (srcSpan loop) accessAnalysis)] ) [] (foldl (\accum item -> accum ++ extractVarNames item) [] reductionVariables))) -- List of variables that are considered 'reduction variables' along with their initial values
											(removeLoopConstructs_recursive loop) -- Body of kernel code

--	An iterative reduction (name change pending) occurs when a parallel reduction occurs in some nested loops but requires values from some outer, iterative loop. More advanced loop carried dependency
--	analysis caused this to be necessary.
paralleliseLoop_iterativeReduce :: String -> Fortran Anno -> Maybe(Fortran Anno) -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarDependencyAnalysis -> VarAccessAnalysis -> (Bool, Fortran Anno)
paralleliseLoop_iterativeReduce filename loop Nothing loopVarNames nonTempVars prexistingVars dependencies accessAnalysis 	|	nextFor_maybe == Nothing = (False, loop)
																														 	|	otherwise = (iterativeReduceAttempt_bool, newAst)
		where
			nextFor_maybe = extractFirstChildFor loop
			(priorFortran, nextFor, followingFortran) = case nextFor_maybe of 
							Nothing -> error "paralleliseLoop_iterativeReduce: nextFor_maybe is Nothing"
							Just a -> a
			(iterativeReduceAttempt_bool, iterativeReduceAttempt_ast) = paralleliseLoop_iterativeReduce filename loop (Just(nextFor)) loopVarNames nonTempVars prexistingVars dependencies accessAnalysis
			newAst = case iterativeReduceAttempt_ast of
						For a1 a2 a3 a4 a5 a6 fortran -> For a1 a2 a3 a4 a5 a6 (appendFortran_recursive (appendFortran_recursive followingFortran fortran) priorFortran)

paralleliseLoop_iterativeReduce filename iteratingLoop (Just(parallelLoop)) loopVarNames nonTempVars prexistingVars dependencies accessAnalysis 
				| 	errors_reduce' == nullAnno 	=	(True, appendAnnotation iterativeReductionCode (compilerName ++ ": Iterative Reduction at " ++ errorLocationFormatting (srcSpan iteratingLoop) ++ " with parallal loop at "  ++ errorLocationFormatting (srcSpan parallelLoop)) "")
				|	nextFor_maybe /= Nothing 	= 	paralleliseLoop_iterativeReduce filename (appendAnnotationMap iteratingLoop errors_reduce') (Just(nextFor)) loopVarNames nonTempVars prexistingVars dependencies accessAnalysis 
				|	otherwise					=	(False, appendAnnotationMap iteratingLoop errors_reduce')

		where
			loopWrites = extractWrites_query parallelLoop
			loopAnalysis = analyseLoop_reduce iterativeReduceComment [] [] loopWrites nonTempVars prexistingVars dependencies accessAnalysis parallelLoop 
			errors_reduce = getErrorAnnotations loopAnalysis
			reductionVariables = getReductionVars loopAnalysis
			reads_reduce = getReads loopAnalysis
			writes_reduce = getWrites loopAnalysis

			iteratingLoopVars = listSubtract (extractLoopVars iteratingLoop) (extractLoopVars parallelLoop)

			(loopCarriedDeps_bool, evaluated_bool, loopCarriedDeps) = loopCarriedDependencyCheck_iterative iteratingLoop parallelLoop
			errors_reduce' = case loopCarriedDeps_bool of
															True -> case evaluated_bool of
																	True -> DMap.insert (outputTab ++ "Cannot reduce: Loop carried dependency detected:\n") (formatLoopCarriedDependencies loopCarriedDeps) errors_reduce
																	False -> DMap.insert (outputTab ++ "Cannot reduce: Loop carried dependency possible (not evaluated):\n") (formatLoopCarriedDependencies loopCarriedDeps) errors_reduce
															False -> errors_reduce

			loopVariables = loopCondtions_query parallelLoop

			startVarNames = foldl (\accum (_,x,_,_) -> accum ++ extractVarNames x) [] loopVariables
			endVarNames = foldl (\accum (_,_,x,_) -> accum ++ extractVarNames x) [] loopVariables
			stepVarNames = foldl (\accum (_,_,_,x) -> accum ++ extractVarNames x) [] loopVariables

			varNames_loopVariables = listSubtract (listRemoveDuplications (startVarNames ++ endVarNames ++ stepVarNames)) loopVarNames

			nextFor_maybe = extractFirstChildFor parallelLoop
			(priorFortran, nextFor, followingFortran) = case nextFor_maybe of 
							Nothing -> error "paralleliseLoop_iterativeReduce: nextFor is Nothing"
							Just a -> a
			newAst = appendFortran_recursive followingFortran (appendFortran_recursive reductionCode priorFortran)

			iterativeReductionCode = case iteratingLoop of
										For a1 a2 a3 a4 a5 a6 fortran -> For a1 a2 a3 a4 a5 a6 newAst -- reductionCode
										_ -> error "paralleliseLoop_iterativeReduce: iterating loop is not FOR"

			reductionCode = OpenCLReduce nullAnno (generateSrcSpan filename (srcSpan parallelLoop))  
						(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames reads_reduce)) (map (\(a, _, _, _) -> a) (loopCondtions_query parallelLoop)) ++ varNames_loopVariables ++ iteratingLoopVars) -- List of arguments to kernel that are READ
		 				(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames writes_reduce)) (map (\(a, _, _, _) -> a) (loopCondtions_query parallelLoop))) -- List of arguments to kernel that are WRITTEN
						(loopVariables) -- Loop variables of nested maps
						(listRemoveDuplications (foldl (\accum item -> accum ++ [(item, getValueAtSrcSpan item (srcSpan parallelLoop) accessAnalysis)] ) [] (foldl (\accum item -> accum ++ extractVarNames item) [] reductionVariables))) -- List of variables that are considered 'reduction variables' along with their initial values
						(removeLoopConstructs_recursive parallelLoop) -- Body of kernel code
			iterativeReduceComment = "Cannot iterative reduce (iter:" ++ (errorLocationFormatting $ srcSpan iteratingLoop) ++ ", par:" ++ (errorLocationFormatting $ srcSpan parallelLoop) ++ "): "

--	Function uses a SYB query to get all of the loop condtions contained within a particular AST. loopCondtions_query traverses the AST
--	and calls getLoopConditions when a Fortran node is encountered.
loopCondtions_query :: (Typeable p, Data p) =>  Fortran p -> [(VarName p, Expr p, Expr p, Expr p)]
loopCondtions_query = everything (++) (mkQ [] getLoopConditions)

getLoopConditions :: (Typeable p, Data p) => Fortran p -> [(VarName p, Expr p, Expr p, Expr p)]
getLoopConditions codeSeg = case codeSeg of
		For _ _ var start end step _ -> [(var, start, end, step)]
		OpenCLMap _ _ _ _ loopVars _ -> loopVars
		OpenCLReduce _ _ _ _ loopVars _ _ -> loopVars
		_ -> []

--	Traverses the AST and prooduces a single string that contains all of the parallelising errors for this particular run of the compiler.
--	compileAnnotationListing traverses the AST and applies getAnnotations to Fortran nodes (as currently they are the only nodes that have
--	ever have annotations applied. The resulting string is then output to the user.
-- compileAnnotationListing :: Program Anno -> String
compileAnnotationListing codeSeg = everything (++) (mkQ [] getAnnotations) codeSeg

getAnnotations :: Fortran Anno -> String
getAnnotations codeSeg = case (tag codeSeg) == nullAnno of
	True -> ""
	False -> leadMessageCheck
		where
			leadMessageCheck = case DMap.findWithDefault [] (head keys) errorMap == [""] of
				True -> errorListing
				False -> leadMessage ++ errorListing ++ "\n"
			leadMessage = compilerName ++ ": Cannot parallelise loop at " ++ errorLocationFormatting (srcSpan codeSeg) ++ "\n"
			errorListing = foldl (\errorDescription key -> errorDescription ++ key ++ 
												(foldl (\errorInstance item -> errorInstance ++ "\t" ++ item) "" (applyAnnotationFormatting 2 (listRemoveDuplications (DMap.findWithDefault [] key errorMap)))) ++ "\n"
					) "" keys 
			keys = DMap.keys errorMap
			errorMap = tag codeSeg

applyAnnotationFormatting :: Int -> [String] -> [String]
applyAnnotationFormatting itemsPerLine items = formattedList
			where
				indexorList = [1..(length items)]
				indexoredItems = zip indexorList items
				formattedList = map (\(index, item) -> if (mod index itemsPerLine) == 0 && index /= (length items) 
															then item ++ "\n" else item) indexoredItems 

formatLoopCarriedDependencies :: [(Expr Anno, Expr Anno)] -> [String]
formatLoopCarriedDependencies ((readExpr, writtenExpr):exprs) = [outputTab ++ (outputExprFormatting writtenExpr) ++ " -> " ++ (outputExprFormatting readExpr)] ++ formatLoopCarriedDependencies exprs
formatLoopCarriedDependencies [] = []

--	Returns a list of all of the names of variables that are used in a particular AST. getVarNames_query performs the traversal and applies
--	getVarNames at appropriate moments.
getVarNames_query :: (Typeable p, Data p) =>  Fortran p -> [VarName p]
getVarNames_query fortran = everything (++) (mkQ [] getVarNames) fortran

getVarNames :: (Typeable p, Data p) =>  VarName p -> [VarName p]
getVarNames expr = [expr]

--	Function checks whether every Expr in a list is a VarName from another list.
exprListContainsVarNames :: (Typeable p, Data p, Eq p) =>  [Expr p] -> [VarName p] -> Bool
exprListContainsVarNames contains container = all (== True) (everything (++) (mkQ [] (varNameCheck container)) contains)

varNameCheck :: (Typeable p, Data p, Eq p) => [VarName p] -> VarName p -> [Bool]
varNameCheck container contains = [elem contains container]