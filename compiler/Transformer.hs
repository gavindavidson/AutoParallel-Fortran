module Main where

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List
import System.Environment
import System.Process
import System.Directory
import qualified Data.Map as DMap 

import CombineKernels
import VarAccessAnalysis
import VarDependencyAnalysis
import LanguageFortranTools
import CodeEmitter
import LoopAnalysis
import ConstantFolding

main :: IO ()
main = do
	putStr "STUFF TO DO:\n"
	putStr "\t- Check for identity value (reduction)\n"
	putStr "<DONE>\t- Finish kernel emission\n"
	putStr "<DONE>\t- Emit code for final host reductions\n"
	putStr "<HALF>\t- Emit NDRange information to host\n"
	putStr "\t\t- .. in correct format for Wim's compiler\n"
	putStr "<DONE>\t- Produce CPP'd version of code\n"
	putStr "<DONE>\t- Make kernels subroutines\n"
	putStr "<DONE>\t- Unique kernel names\n"
	putStr "<DONE>\t- Add kernels/subroutines to module for each original source file\n"
	putStr "<DONE>\t- Add declarations for arguments to kernels\n"
	putStr "<DONE>\t- Add integer :: g_id, get_global_id(g_id,0) code segments\n"
	putStr "<DONE>\t- Make annotations String -> Anno maps\n"
	putStr "<DONE>\t- Make kernel loop variable calculations deal with different starts\n"
	putStr "<DONE>\t- Update error messages (\"i, j, k were not used\")\n"
	putStr "<WORKS>\t- Test reduce and map combination\n"
	putStr "\t- Array/scaler optimisations\n"
	putStr "<DONE>\t- Make output prettier\n"
	putStr "<TBC>\t- Fix generated kernel code (Wim's email)\n"
	putStr "<DONE>\t- Fix if block generation to include elses\n"
	putStr "\t- Fix VarAccessAnalysis to deal with ifs better\n"
	putStr "<DONE>\t- Subroutine name case issue (velfg.f95)\n"
	putStr "<DONE>\t- Missing \"end subroutine blah\"s\n"
	putStr "\n"

	args <- getArgs
	let argMap = processArgs args

	let filename = DMap.findWithDefault (usageError) filenameFlag argMap
	let newFilename = DMap.lookup outFileFlag argMap
	let loopFusionBound = case DMap.lookup loopFusionBoundFlag argMap of
							Just bound -> Just (read bound :: Float)
							Nothing -> Nothing

	--a <- parseFile "../testFiles/arrayLoop.f95"
	parsedProgram <- parseFile filename
	let parallelisedProg = paralleliseProgram (parsedProgram)
	let combinedProg = combineKernels loopFusionBound (removeAllAnnotations parallelisedProg)

	putStr $ compileAnnotationListing parallelisedProg
	
	putStr "\n"
	putStr $ compileAnnotationListing combinedProg
	-- putStr $ show $ combinedProg
	-- putStr "\n"
	--emit (filename) "" parsedProgram
	
	emit filename newFilename combinedProg

	--emit (filename) "" parallelisedProg
	-- putStr $ show $ parsedProgram
	--putStr "\n\n\n"	

	-- putStr "\n"

	--putStr "\n"

filenameFlag = "filename"
outFileFlag = "-out"
loopFusionBoundFlag = "-lfb"

processArgs :: [String] -> DMap.Map String String
processArgs argList 	| 	even (length argList) = usageError
						| 	(length argList) == 0 = usageError
						|	otherwise = foldl (\accum (flagIndex, valueIndex) -> DMap.insert (argList!!flagIndex) (argList!!valueIndex) accum) mapWithInpFile pairArgs
		where
			mapWithInpFile = DMap.insert filenameFlag (head argList) DMap.empty
			oddArgs = [1,3.. (length argList)-1]
			evenArgs = [2,4.. (length argList)-1]
			pairArgs = zip oddArgs evenArgs

usageError = error "USAGE: <filename> [<flag> <value>]"

--	The top level function that is called against the original parsed AST
paralleliseProgram :: Program Anno -> Program Anno 
paralleliseProgram codeSeg = map (everywhere (mkT (paralleliseBlock (accessAnalysis)))) codeSeg
	where
		accessAnalysis = analyseAllVarAccess codeSeg

--	Called by above to identify actions to take when a Block is encountered. In this case look for loops to parallelise using paralleliseForLoop
paralleliseBlock :: VarAccessAnalysis -> Block Anno -> Block Anno
paralleliseBlock accessAnalysis block = gmapT (mkT (paralleliseForLoop accessAnalysis)) block

--	When a for loop is encountered this function attempts to parallelise it.
paralleliseForLoop :: VarAccessAnalysis -> Fortran Anno -> Fortran Anno
paralleliseForLoop  accessAnalysis inp = case inp of
		For _ _ _ _ _ _ _ -> paralleliseLoop [] accessAnalysis $ gmapT (mkT (paralleliseForLoop accessAnalysis )) inp
		_ -> gmapT (mkT (paralleliseForLoop accessAnalysis)) inp

--	Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new parallel (OpenCLMap etc)
--	nodes or the original sub-tree annotated with parallelisation errors. Attempts to map and then to reduce.
paralleliseLoop :: [VarName Anno] -> VarAccessAnalysis -> Fortran Anno -> Fortran Anno
paralleliseLoop loopVars accessAnalysis loop 	=  -- appendAnnotation (
												case mapAttempt_bool of
										True	-> appendAnnotation mapAttempt_ast (compilerName ++ ": Map at " ++ errorLocationFormatting (srcSpan loop)) ""
										False 	-> case reduceAttempt_bool of
													True 	-> appendAnnotation reduceAttempt_ast (compilerName ++ ": Reduction at " ++ errorLocationFormatting (srcSpan loop)) ""
													False	-> reduceAttempt_ast -- ) ("localVarRecords " ++  errorLocationFormatting (srcSpan loop) ++ "\n\n") (show localVarRecords)
								where
									newLoopVars = case getLoopVar loop of
										Just a -> loopVars ++ [a]
										Nothing -> loopVars

									-- varValueRecords = (\(_,x,_,_) -> x) accessAnalysis
									localVarRecords = (\(x,_,_,_) -> x) accessAnalysis
									nonTempVars = getNonTempVars (srcSpan loop) accessAnalysis
									dependencies = analyseDependencies loop
									loopWrites = extractWrites_query loop

									mapAttempt = paralleliseLoop_map loop newLoopVars loopWrites nonTempVars dependencies accessAnalysis
									mapAttempt_bool = fst mapAttempt
									mapAttempt_ast = snd mapAttempt

									reduceAttempt = paralleliseLoop_reduce mapAttempt_ast newLoopVars loopWrites nonTempVars dependencies accessAnalysis
									reduceAttempt_bool = fst reduceAttempt
									reduceAttempt_ast = snd reduceAttempt

--	These functions are used to extract a list of varnames that are written to in a particular chunk of code. Used to asses
extractWrites_query :: (Typeable p, Data p) => Fortran p -> [VarName p]
extractWrites_query = everything (++) (mkQ [] extractWrites)

extractWrites :: (Typeable p, Data p) => Fortran p -> [VarName p]
extractWrites (Assg _ _ (Var _ _ list) _) = map (\(varname, exprs) -> varname) list
extractWrites _ = []

--	Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new OpenCLMap nodes or the
--	original sub-tree annotated with reasons why the loop cannot be mapped
paralleliseLoop_map :: Fortran Anno -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarDependencyAnalysis -> VarAccessAnalysis -> (Bool, Fortran Anno)
paralleliseLoop_map loop loopVarNames loopWrites nonTempVars dependencies accessAnalysis	|	errors_map == nullAnno 	=	(True,
											OpenCLMap nullAnno (generateSrcSpan (srcSpan loop)) 	-- Node to represent the data needed for an OpenCL map kernel
											(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames reads_map)) (map (\(a, _, _, _) -> a) (loopCondtions_query loop)) ++ varNames_loopVariables)	-- List of arguments to kernel that are READ
							 				(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames writes_map)) (map (\(a, _, _, _) -> a) (loopCondtions_query loop))) 	-- List of arguments to kernel that are WRITTEN
											(loopVariables)	-- Loop variables of nested maps
											(removeLoopConstructs_recursive loop)) -- Body of kernel code

									|	otherwise	=			(False, appendAnnotationMap loop errors_map)
									where
										loopAnalysis = analyseLoop_map loopVarNames loopWrites nonTempVars accessAnalysis dependencies loop
										errors_map = getErrorAnnotations loopAnalysis
										reads_map = getReads loopAnalysis
										writes_map = getWrites loopAnalysis

										loopVariables = loopCondtions_query loop

										startVarNames = foldl (\accum (_,x,_,_) -> accum ++ extractVarNames x) [] loopVariables
										endVarNames = foldl (\accum (_,_,x,_) -> accum ++ extractVarNames x) [] loopVariables
										stepVarNames = foldl (\accum (_,_,_,x) -> accum ++ extractVarNames x) [] loopVariables

										varNames_loopVariables = listSubtract (listRemoveDuplications (startVarNames ++ endVarNames ++ stepVarNames)) loopVarNames

--	Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new OpenCLReduce nodes or the
--	original sub-tree annotated with reasons why the loop is not a reduction
paralleliseLoop_reduce ::Fortran Anno -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarDependencyAnalysis -> VarAccessAnalysis -> (Bool, Fortran Anno)
paralleliseLoop_reduce loop loopVarNames loopWrites nonTempVars dependencies accessAnalysis	| errors_reduce == nullAnno 	=	(True, 
											OpenCLReduce nullAnno (generateSrcSpan (srcSpan loop))  
 											(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames reads_reduce)) (map (\(a, _, _, _) -> a) (loopCondtions_query loop)) ++ varNames_loopVariables) -- List of arguments to kernel that are READ
							 				(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames writes_reduce)) (map (\(a, _, _, _) -> a) (loopCondtions_query loop))) -- List of arguments to kernel that are WRITTEN
											(loopVariables) -- Loop variables of nested maps
											(listRemoveDuplications (foldl (\accum item -> accum ++ [(item, getValueAtSrcSpan item (srcSpan loop) accessAnalysis)] ) [] (foldl (\accum item -> accum ++ extractVarNames item) [] reductionVariables))) -- List of variables that are considered 'reduction variables' along with their initial val
											(removeLoopConstructs_recursive loop)) -- Body of kernel code

									|	otherwise				=	(False, appendAnnotationMap loop errors_reduce)
									where
										loopAnalysis = analyseLoop_reduce [] loopVarNames loopWrites nonTempVars dependencies accessAnalysis loop 
										errors_reduce = getErrorAnnotations loopAnalysis
										reductionVariables = getReductionVars loopAnalysis
										reads_reduce = getReads loopAnalysis
										writes_reduce = getWrites loopAnalysis

										loopVariables = loopCondtions_query loop

										startVarNames = foldl (\accum (_,x,_,_) -> accum ++ extractVarNames x) [] loopVariables
										endVarNames = foldl (\accum (_,_,x,_) -> accum ++ extractVarNames x) [] loopVariables
										stepVarNames = foldl (\accum (_,_,_,x) -> accum ++ extractVarNames x) [] loopVariables

										varNames_loopVariables = listSubtract (listRemoveDuplications (startVarNames ++ endVarNames ++ stepVarNames)) loopVarNames



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
compileAnnotationListing :: Program Anno -> String
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
												(foldl (\errorInstance item -> errorInstance ++ "\t" ++ item) "" (applyAnnotationFormatting 3 (DMap.findWithDefault [] key errorMap))) ++ "\n"
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

-- 	Function returns the loop variable for an AST representing a for loop
getLoopVar :: Fortran p -> Maybe(VarName p)
getLoopVar (For _ _ var _ _ _ _) = Just var
getLoopVar _ = Nothing
