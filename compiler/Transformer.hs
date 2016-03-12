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

--	Type used to standardise loop analysis functions
--						errors 		reduction variables read variables		written variables		
type AnalysisInfo = 	(Anno, 	[Expr Anno], 		[Expr Anno], 		[Expr Anno])

analysisInfoBaseCase :: AnalysisInfo
analysisInfoBaseCase = (nullAnno,[],[],[])

combineAnalysisInfo :: AnalysisInfo -> AnalysisInfo -> AnalysisInfo
combineAnalysisInfo accum item = (combineMaps accumErrors itemErrors, accumReductionVars ++ itemReductionVars, accumReads ++ itemReads, accumWrites ++ itemWrites)
								where
									(accumErrors, accumReductionVars, accumReads, accumWrites) = accum
									(itemErrors, itemReductionVars, itemReads, itemWrites) = item

-- 3/

-- module_kernels_press_out.f95:16.34:

--     real(kind=4), Dimension((-1):(ip + 1)), Intent(In) :: dx1
--                                   1
-- Error: Variable 'ip' cannot appear in the expression at (1)

-- ip/jp/kp should be declared statically, currently they are not declared at all

-- 4/

-- module_kernels_press_out.f95:26.30:

--     real(kind=4), Dimension(1:num_groups), Intent(Out) :: global_rhsav_array
--                               1
-- Error: Variable 'num_groups' cannot appear in the expression at (1)

-- You can't use a run-time computed value to dimension an array, so num_groups must become a compile_time constant. So these should not be API calls. I suggest you replace them by macros that would become constants constants. Same for group_size.

main :: IO ()
main = do
	--a <- parseFile "../testFiles/arrayLoop.f95"
	
	putStr "STUFF TO DO:\n"
	putStr "\t- Check for identity value (reduction)\n"
	putStr "<DONE>\t- Finish kernel emission\n"
	putStr "<DONE>\t- Emit code for final host reductions\n"
	putStr "<DONE>\t- Emit NDRange information to host\n"
	putStr "<DONE>\t- Produce CPP'd version of code\n"
	putStr "<DONE>\t- Make kernels subroutines\n"
	putStr "<DONE>\t- Unique kernel names\n"
	putStr "<DONE>\t- Add kernels/subroutines to module for each original source file\n"
	putStr "<DONE>\t- Add declarations for arguments to kernels\n"
	putStr "<DONE>\t- Add integer :: g_id, get_global_id(g_id,0) code segments\n"
	putStr "<DONE>\t- Make annotations String -> Anno maps\n"
	putStr "\t- Make kernel loop variable calculations deal with different starts\n"
	putStr "\t- Update error messages (\"i, j, k were not used\")\n"
	putStr "<WORKS>\t- Test reduce and map combination\n"
	putStr "\t- Array/scaler optimisations\n"
	putStr "<DONE>\t- Make output prettier\n"
	putStr "\t- Fix generated kernel code (Wim's email)\n"
	putStr "\t- Fix if block generation to include elses\n"
	putStr "\t- Fix VarAccessAnalysis to deal with ifs better\n"

	putStr "\n"

	args <- getArgs
	let filename = args!!0
	let newFilename = case (length args > 1) of
						True -> args!!1
						False -> ""

	--a <- parseFile "../testFiles/arrayLoop.f95"
	parsedProgram <- parseFile filename
	let parallelisedProg = paralleliseProgram (parsedProgram)
	let combinedProg = combineKernels (removeAllAnnotations parallelisedProg)

	-- cppd <- cpp filename
	--putStr (cppd)

	putStr $ compileAnnotationListing parallelisedProg
	
	putStr "\n"
	putStr $ compileAnnotationListing combinedProg
	putStr $ show $ combinedProg
	-- putStr "\n"
	--emit (filename) "" parsedProgram
	emit filename newFilename combinedProg
	--emit (filename) "" parallelisedProg
	-- putStr $ show $ parsedProgram
	--putStr "\n\n\n"	

	-- putStr "\n"

	--putStr "\n"

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
paralleliseLoop :: [VarName Anno] -> VarAccessAnalysis ->Fortran Anno -> Fortran Anno
paralleliseLoop loopVars accessAnalysis loop 	=  appendAnnotation (
												case mapAttempt_bool of
										True	-> appendAnnotation mapAttempt_ast (compilerName ++ ": Map at " ++ errorLocationFormatting (srcSpan loop)) ""
										False 	-> case reduceAttempt_bool of
													True 	-> appendAnnotation reduceAttempt_ast (compilerName ++ ": Reduction at " ++ errorLocationFormatting (srcSpan loop)) ""
													False	-> reduceAttempt_ast ) ("localVarRecords " ++  errorLocationFormatting (srcSpan loop) ++ "\n\n") (show localVarRecords)
								where
									newLoopVars = case getLoopVar loop of
										Just a -> loopVars ++ [a]
										Nothing -> loopVars

									-- varValueRecords = (\(_,x,_,_) -> x) accessAnalysis
									localVarRecords = (\(x,_,_,_) -> x) accessAnalysis
									nonTempVars = getNonTempVars (srcSpan loop) accessAnalysis
									dependencies = analyseDependencies accessAnalysis loop
									loopWrites = extractWrites_query loop

									mapAttempt = paralleliseLoop_map loop newLoopVars loopWrites nonTempVars accessAnalysis
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
paralleliseLoop_map :: Fortran Anno -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarAccessAnalysis -> (Bool, Fortran Anno)
paralleliseLoop_map loop loopVarNames loopWrites nonTempVars accessAnalysis	|	errors_map == nullAnno 	=	(True,
											OpenCLMap nullAnno (generateSrcSpan (srcSpan loop)) 	-- Node to represent the data needed for an OpenCL map kernel
											(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames reads_map)) (map (\(a, _, _, _) -> a) (loopCondtions_query loop)) ++ varNames_loopVariables)	-- List of arguments to kernel that are READ
							 				(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames writes_map)) (map (\(a, _, _, _) -> a) (loopCondtions_query loop))) 	-- List of arguments to kernel that are WRITTEN
											(loopVariables)	-- Loop variables of nested maps
											(removeLoopConstructs_recursive loop)) -- Body of kernel code

									|	otherwise	=			(False, appendAnnotationMap loop errors_map)
									where
										(errors_map, _, reads_map, writes_map) = analyseLoop_map loopVarNames loopWrites nonTempVars accessAnalysis loop
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
										(errors_reduce, reductionVariables, reads_reduce, writes_reduce) = analyseLoop_reduce [] loopVarNames loopWrites nonTempVars dependencies accessAnalysis loop 
										loopVariables = loopCondtions_query loop

										startVarNames = foldl (\accum (_,x,_,_) -> accum ++ extractVarNames x) [] loopVariables
										endVarNames = foldl (\accum (_,_,x,_) -> accum ++ extractVarNames x) [] loopVariables
										stepVarNames = foldl (\accum (_,_,_,x) -> accum ++ extractVarNames x) [] loopVariables

										varNames_loopVariables = listSubtract (listRemoveDuplications (startVarNames ++ endVarNames ++ stepVarNames)) loopVarNames

--	Function takes a list of loop variables and a possible parallel loop's AST and returns a string that details the reasons why the loop
--	cannot be mapped. If the returned string is empty, the loop represents a possible parallel map
analyseLoop_map :: [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarAccessAnalysis -> Fortran Anno -> AnalysisInfo
analyseLoop_map loopVars loopWrites nonTempVars accessAnalysis codeSeg = case codeSeg of
		Assg _ srcspan expr1 expr2 -> combineAnalysisInfo (combineAnalysisInfo expr1Analysis expr2Analysis) (nullAnno,[],expr2Operands,[expr1])
						where
							expr1Analysis = (analyseAccess_map loopVars loopWrites nonTempVars accessAnalysis expr1)
							expr2Analysis = (analyseAccess_map loopVars loopWrites nonTempVars accessAnalysis expr2)
							expr2Operands = extractOperands expr2
		For _ _ var _ _ _ _ -> foldl combineAnalysisInfo analysisInfoBaseCase childrenAnalysis
						where
							childrenAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_map (loopVars ++ [var]) loopWrites nonTempVars accessAnalysis)) codeSeg)
		_ -> foldl combineAnalysisInfo analysisInfoBaseCase (childrenAnalysis ++ nodeAccessAnalysis)
						where
							nodeAccessAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseAccess_map loopVars loopWrites nonTempVars accessAnalysis)) codeSeg)
							childrenAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_map loopVars loopWrites nonTempVars accessAnalysis)) codeSeg)

--	Function takes a list of loop variables and a possible parallel loop's AST and returns a string that details the reasons why the loop
--	doesn't represent a reduction. If the returned string is empty, the loop represents a possible parallel reduction
analyseLoop_reduce :: [Expr Anno] -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarDependencyAnalysis -> VarAccessAnalysis -> Fortran Anno -> AnalysisInfo
analyseLoop_reduce condExprs loopVars loopWrites nonTempVars dependencies accessAnalysis codeSeg = case codeSeg of
		If _ _ expr _ _ _ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce (condExprs ++ [expr]) loopVars loopWrites nonTempVars dependencies accessAnalysis)) codeSeg)
		For _ _ var _ _ _ _ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce condExprs (loopVars ++ [var]) loopWrites nonTempVars dependencies accessAnalysis)) codeSeg)
		Assg _ srcspan expr1 expr2 -> 	combineAnalysisInfo (
											errorMap3
											,
											if potentialReductionVar then [expr1] else [],
											extractOperands expr2,
											[expr1])
											usesFullLoopVarError
			where
				writtenExprs = extractOperands expr1
				readOperands = extractOperands expr2
				readExprs = foldl (\accum item -> if isFunctionCall accessAnalysis item then accum ++ (extractContainedVars item) else accum ++ [item]) [] readOperands

				dependsOnSelfOnce = foldl (/=) False (map (\y -> foldl (||) False $ (map (\x -> isIndirectlyDependentOn dependencies y x) writtenVarnames)) readVarnames)

				writtenVarnames = foldl (\accum item -> accum ++ extractVarNames item) [] writtenExprs
				readVarnames 	= foldl (\accum item -> accum ++ extractVarNames item) [] readExprs

				isNonTempAssignment = hasVarName nonTempVars expr1
				referencedCondition = (foldl (||) False $ map (\x -> hasOperand x expr1) condExprs)
				referencedSelf = (hasOperand expr2 expr1)
				associative = isAssociativeExpr expr1 expr2
				dependsOnSelf = dependsOnSelfOnce || (foldl (||) False $ map (\x -> isIndirectlyDependentOn dependencies x x) writtenVarnames)
				--usesFullLoopVarError = analyseAccess_reduce loopVars loopWrites nonTempVars accessAnalysis expr1
				usesFullLoopVarError = analyseAccess_reduce loopVars loopWrites nonTempVars accessAnalysis expr1


				potentialReductionVar = isNonTempAssignment && (referencedSelf || referencedCondition || dependsOnSelf) && ((\(errorMap, _, _, _) -> errorMap == nullAnno) usesFullLoopVarError)
				--potentialReductionVar = isNonTempAssignment && (referencedSelf || referencedCondition) && ((\(str, _, _, _) -> str == "") usesFullLoopVarError)

				errorMap1 = if (not potentialReductionVar) && isNonTempAssignment then 
											DMap.insert (outputTab ++ "Cannot reduce: variables that do not use all loop iterators cannot be reduction variables beacause they are not " ++
													"assigned values related to themselves and do not appear in a preceeding conditional constructs:\n" )
												[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr1]
												DMap.empty
											else DMap.empty
				errorMap2 = if potentialReductionVar && (not dependsOnSelfOnce) then
											DMap.insert (outputTab ++ "Cannot reduce: possible reduction variables must only appear once on the right hand side of an assignment:\n")
												[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr1]
												errorMap1
											else errorMap1
				errorMap3 = if potentialReductionVar && (not associative) && dependsOnSelfOnce then
											DMap.insert (outputTab ++ "Cannot reduce: not associative function:\n")
												[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr2]
												errorMap2
											else errorMap2
				errorMap_debug = if referencedSelf then
											DMap.insert (outputTab ++ "References self:\n")
												[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr1]
												errorMap3
											else errorMap3
				errorMap_debug' = if isNonTempAssignment then
											DMap.insert (outputTab ++ "Non temp assignments:\n")
												[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr1]
												errorMap_debug
											else errorMap_debug
		Call _ srcspan expr arglist -> (errorMap_call, [], [], argExprs)
			where
				errorMap_call = DMap.insert (outputTab ++ "Cannot reduce: Call to external function:\n")
												[errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr]
												DMap.empty
				argExprs = everything (++) (mkQ [] extractExpr_list) arglist

		_ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce condExprs loopVars loopWrites nonTempVars dependencies accessAnalysis)) codeSeg)	

--	Determines whether or not an expression contains a non temporary variable and whether it is access correctly for a map. Produces and appropriate string error
analyseAccess_map :: [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarAccessAnalysis -> Expr Anno -> AnalysisInfo
analyseAccess_map loopVars loopWrites nonTempVars accessAnalysis expr = (errorMap, [],[],[]) --(errors, [],[],[])
								where
									operands = case fnCall of
											True ->	extractContainedVars expr
											False -> extractOperands expr
									writtenOperands = filter (hasVarName loopWrites) operands
									fnCall = isFunctionCall accessAnalysis expr
									nonTempWrittenOperands = filter(hasVarName nonTempVars) writtenOperands
									
									badExprs =  filter (\item -> listSubtract loopVars (foldl (\accum item -> accum ++ extractVarNames item) [] (extractContainedVars item)) /= []) nonTempWrittenOperands
									badExprStrs = map (\item -> errorLocationFormatting (srcSpan item) ++ outputTab ++ outputExprFormatting item) badExprs

									errorMap = if (badExprStrs == []) 
												then nullAnno
												else 
													DMap.insert (outputTab ++ "Cannot map: Non temporary, write variables accessed without full use of loop iterator:\n") badExprStrs nullAnno


--	Determines whether or not an expression contains a non temporary variable and whether it is access correctly for a reduction. Produces and appropriate string error
analyseAccess_reduce :: [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarAccessAnalysis -> Expr Anno -> AnalysisInfo
analyseAccess_reduce loopVars loopWrites nonTempVars accessAnalysis expr = (errorMap, [],[],[]) --(errors, [],[],[])
								where
									operands = case fnCall of
											True ->	extractContainedVars expr
											False -> extractOperands expr
									writtenOperands = filter (hasVarName loopWrites) operands
									fnCall = isFunctionCall accessAnalysis expr
									nonTempWrittenOperands = filter(hasVarName nonTempVars) writtenOperands
									
									badExprs =  filter (\item -> listSubtract loopVars (foldl (\accum item -> accum ++ extractVarNames item) [] (extractContainedVars item)) == []) nonTempWrittenOperands
									badExprStrs = map (\item -> errorLocationFormatting (srcSpan item) ++ outputTab ++ outputExprFormatting item) badExprs

									errorMap = if (badExprStrs == []) 
												then nullAnno
												else 
													DMap.insert (outputTab ++ "Cannot reduce: Non temporary, write variables written to without full use of loop iterator:\n") badExprStrs nullAnno

--	Function checks whether the primary in a reduction assignmnet is an associative operation. Checks both associative ops and functions.
isAssociativeExpr :: Expr Anno -> Expr Anno -> Bool
isAssociativeExpr assignee assignment = case assignment of
							(Bin _ _ op expr1 expr2) -> associativeOp
							_ -> associativeFunc
						where 
							primaryOp = extractPrimaryReductionOp assignee assignment
							primaryFunc = extractPrimaryReductionFunction assignee assignment
							associativeOp = case primaryOp of
												Just oper -> isAssociativeOp oper
												Nothing -> False
							associativeFunc = isAssociativeFunction primaryFunc

isAssociativeOp :: BinOp Anno -> Bool
isAssociativeOp (Plus p) = True
isAssociativeOp (Mul p) = True
isAssociativeOp (Or p) = True
isAssociativeOp _ = False

--	Not yet used. In future the program may be able to detect whether or not a variable is given an appropriate start value for a reduction
opIdentityValue :: BinOp Anno -> Expr Anno
opIdentityValue (Plus p) = Con nullAnno nullSrcSpan "0"
opIdentityValue (Mul p) = Con nullAnno nullSrcSpan "1"
opIdentityValue (Or p) = Con nullAnno nullSrcSpan ".FALSE."
opIdentityValue _ = Null nullAnno nullSrcSpan

isAssociativeFunction :: String -> Bool
isAssociativeFunction fnName = case (map (toLower) fnName) of
								"min" -> True
								"max" -> True
								"amax1" -> True
								"amin1" -> True
								_ -> False

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

--	Value used as a global spacing measure. Used for output formatting.
outputTab :: String
outputTab = "  "
