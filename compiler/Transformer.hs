module Main where

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List
import System.Environment
import System.Process
import System.Directory

import PreProcessor
import VarAccessAnalysis
import VarDependencyAnalysis
import LanguageFortranTools

--	Type used to standardise loop analysis functions
--						errors 		reduction variables read variables		written variables		
type AnalysisInfo = 	(String, 	[Expr [String]], 	[Expr [String]], 	[Expr [String]])

main :: IO ()
main = do
	--a <- parseFile "../testFiles/arrayLoop.f95"
	
	args <- getArgs
	let filename = args!!0

	--a <- parseFile "../testFiles/arrayLoop.f95"
	parsedProgram <- parseFile filename
	let parallelisedProg = paralleliseProgram (parsedProgram)

	putStr $ compileErrorListing parallelisedProg
	putStr "\n"

	putStr $ show $ parsedProgram
	putStr "\n\n\n"

	--putStr $ show $ parallelisedProg
	--putStr "\n"

	--putStr "\n"

--	Taken from language-fortran example. Runs preprocessor on target source and then parses the result, returning an AST.
parseFile s = do inp <- readProcess "cpp" [s, "-D", "NO_IO", "-P"] "" 
                 return $ parse $ preProcess inp
                 --return $ preProcess inp

--	Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new parallel (OpenCLMap etc)
--	nodes or the original sub-tree annotated with parallelisation errors.
paralleliseLoop :: [VarName [String]] -> VarAccessAnalysis ->Fortran [String] -> Fortran [String]
paralleliseLoop loopVars accessAnalysis loop 	= case mapAttempt_bool of
										True	-> mapAttempt_ast 
										False 	-> case reduceAttempt_bool of
													True 	-> reduceAttempt_ast
													False	-> addAnnotation (reduceAttempt_ast) (show $ accessAnalysis)
								--case paralleliseLoop_map loop newLoopVars of 
								--	Just a -> a
								--	Nothing -> loop
								where
									newLoopVars = case getLoopVar loop of
										Just a -> loopVars ++ [a]
										Nothing -> loopVars

									nonTempVars = getNonTempVars (srcSpan loop) accessAnalysis
									dependencies = analyseDependencies loop
									loopWrites = extractWrites_query loop

									mapAttempt = paralleliseLoop_map loop newLoopVars loopWrites nonTempVars accessAnalysis
									mapAttempt_bool = fst mapAttempt
									mapAttempt_ast = snd mapAttempt

									reduceAttempt = paralleliseLoop_reduce mapAttempt_ast newLoopVars loopWrites nonTempVars dependencies accessAnalysis
									reduceAttempt_bool = fst reduceAttempt
									reduceAttempt_ast = snd reduceAttempt

extractWrites_query :: (Typeable p, Data p) => Fortran p -> [VarName p]
extractWrites_query = everything (++) (mkQ [] extractWrites)

extractWrites :: (Typeable p, Data p) => Fortran p -> [VarName p]
extractWrites (Assg _ _ (Var _ _ list) _) = map (\(varname, exprs) -> varname) list
extractWrites _ = []

--	Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new OpenCLMap nodes or the
--	original sub-tree annotated with reasons why the loop cannot be mapped
paralleliseLoop_map :: Fortran [String] -> [VarName [String]] -> [VarName [String]] -> [VarName [String]] -> VarAccessAnalysis -> (Bool, Fortran [String])
paralleliseLoop_map loop loopVars loopWrites nonTempVars accessAnalysis	|	errors_map == "" 	=	(True,
																	OpenCLMap [outputTab ++ "Map found at " ++ errorLocationFormatting (srcSpan loop)] generatedSrcSpan 
													 				--(listRemoveDuplications (listSubtract (getVarNames_query loop) (Prelude.map (\(a, _, _, _) -> a) (loopCondtions_query loop))) ) 
													 				(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames reads_map)) (map (\(a, _, _, _) -> a) (loopCondtions_query loop)))
													 				(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames writes_map)) (map (\(a, _, _, _) -> a) (loopCondtions_query loop)))
																	--(flattenLoopConditions Nothing (VarName [] "g_id") 
																	(loopCondtions_query loop) --)   
																	--(removeLoopConstructs_trans loop))
																	(removeLoopConstructs_recursive loop))
									|	otherwise	=			(False, addAnnotation loop (outputTab ++ "Cannot map due to:\n" ++ errors_map))
									where
										(errors_map, _, reads_map, writes_map) = analyseLoop_map loopVars loopWrites nonTempVars accessAnalysis loop

--	Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new OpenCLReduce nodes or the
--	original sub-tree annotated with reasons why the loop is not a reduction
paralleliseLoop_reduce ::Fortran [String] -> [VarName [String]] -> [VarName [String]] -> [VarName [String]] -> VarDependencyAnalysis -> VarAccessAnalysis -> (Bool, Fortran [String])
paralleliseLoop_reduce loop loopVars loopWrites nonTempVars dependencies accessAnalysis	|	errors_reduce == "" 	=	(True, addAnnotation loop (outputTab ++ "Reduction found at " ++ errorLocationFormatting (srcSpan loop)))
															|	otherwise				=	(False, addAnnotation loop (outputTab ++ "Cannot reduce due to:\n" ++ errors_reduce))
									where
										(errors_reduce, reductionVariables, reads_reduce, writes_reduce) = analyseLoop_reduce [] loopVars loopWrites nonTempVars dependencies accessAnalysis loop 

--	Function takes a list of loop variables and a possible parallel loop's AST and returns a string that details the reasons why the loop
--	cannot be mapped. If the returned string is empty, the loop represents a possible parallel map
analyseLoop_map :: [VarName [String]] -> [VarName [String]] -> [VarName [String]] -> VarAccessAnalysis -> Fortran [String] -> AnalysisInfo
analyseLoop_map loopVars loopWrites nonTempVars accessAnalysis codeSeg = case codeSeg of
		Assg _ srcspan expr1 expr2 -> combineAnalysisInfo (combineAnalysisInfo expr1Analysis expr2Analysis) ("",[],[],[expr1])
						where
							expr1Analysis = (analyseAccess_map loopVars loopWrites nonTempVars accessAnalysis expr1)
							expr2Analysis = (analyseAccess_map loopVars loopWrites nonTempVars accessAnalysis expr2)
		For _ _ var _ _ _ _ -> foldl combineAnalysisInfo analysisInfoBaseCase childrenAnalysis
						where
							childrenAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_map (loopVars ++ [var]) loopWrites nonTempVars accessAnalysis)) codeSeg)
		_ -> foldl combineAnalysisInfo analysisInfoBaseCase (childrenAnalysis ++ nodeAccessAnalysis)
						where
							nodeAccessAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseAccess_map loopVars loopWrites nonTempVars accessAnalysis)) codeSeg)
							childrenAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_map loopVars loopWrites nonTempVars accessAnalysis)) codeSeg)

--analyseLoop_reduce :: [VarName [String]] -> Fortran [String] -> (String, [Expr [String]], [Expr [String]])
analyseLoop_reduce :: [Expr [String]] -> [VarName [String]] -> [VarName [String]] -> [VarName [String]] -> VarDependencyAnalysis -> VarAccessAnalysis -> Fortran [String] -> AnalysisInfo
analyseLoop_reduce condExprs loopVars loopWrites nonTempVars dependencies accessAnalysis codeSeg = case codeSeg of
		Assg _ srcspan expr1 expr2 -> 	(
										(if (not potentialReductionVar) && isNonTempAssignment then 
											outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": possible reduction variable (" 
												++ errorExprFormatting expr1 ++ ") is not assigned a value related to itself and does not appear in a preceeding conditional construct\n" else "") 
										,
										if potentialReductionVar then [expr1] else [],
										extractOperands expr2,
										[expr1])
			where
				--assignments = arrayAccesses_query expr1
				--accesses = arrayAccesses_query expr2
				operands = extractOperands expr1
				writtenVarnames = foldl (\accum item -> accum ++ extractVarNames item) [] operands
				isNonTempAssignment = hasVarName nonTempVars expr1
				referencedCondition = (foldl (||) False $ map (\x -> hasOperand x expr1) condExprs)
				referencedSelf = (hasOperand expr2 expr1)
				dependsOnSelf = (foldl (||) False $ map (\x -> isIndirectlyDependentOn dependencies x x) writtenVarnames) --isIndirectlyDependentOn dependencies 
				
				potentialReductionVar = isNonTempAssignment && (referencedSelf || referencedCondition || dependsOnSelf) 

		If _ _ expr _ _ _ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce (condExprs ++ [expr]) loopVars loopWrites nonTempVars dependencies accessAnalysis)) codeSeg)
		For _ _ var _ _ _ _ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce condExprs (loopVars ++ [var]) loopWrites nonTempVars dependencies accessAnalysis)) codeSeg)
		_ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce condExprs loopVars loopWrites nonTempVars dependencies accessAnalysis)) codeSeg)

analysisInfoBaseCase :: AnalysisInfo
analysisInfoBaseCase = ("",[],[],[])

combineAnalysisInfo :: AnalysisInfo -> AnalysisInfo -> AnalysisInfo
combineAnalysisInfo accum item = (accumErrors ++ itemErrors, accumReductionVars ++ itemReductionVars, accumReads ++ itemReads, accumWrites ++ itemWrites)
								where
									(accumErrors, accumReductionVars, accumReads, accumWrites) = accum
									(itemErrors, itemReductionVars, itemReads, itemWrites) = item



hasOperand :: Expr [String] -> Expr [String] -> Bool
hasOperand container contains = all (== True) $ map (\x -> elem x (extractOperands $ standardiseSrcSpan_trans container)) (extractOperands $ standardiseSrcSpan_trans contains)

--	Appends a new item to the list of annotations already associated to a particular node
addAnnotation :: Fortran [String] -> String -> Fortran [String]
addAnnotation original appendage = case original of
		For anno srcspan var expr1 expr2 expr3 fortran -> For (anno ++ [appendage]) srcspan var expr1 expr2 expr3 fortran
		_ -> original		

analyseAccess_map :: [VarName [String]] -> [VarName [String]] -> [VarName [String]] -> VarAccessAnalysis -> Expr [String] -> AnalysisInfo
analyseAccess_map loopVars loopWrites nonTempVars accessAnalysis expr = (errors, [],[expr],[])
								where
									operands = extractOperands expr
									writtenOperands = filter (hasVarName loopWrites) operands
									fnCall = isFunctionCall accessAnalysis expr
									--nonTempWrittenOperands = filter (\x -> not $ hasVarName nonTempVars x) writtenOperands
									nonTempWrittenOperands = filter(hasVarName nonTempVars) writtenOperands
									errors = foldl (++) "" $ map (\item -> 

																if listSubtract loopVars (foldl (\accum item -> accum ++ extractVarNames item) [] (extractContainedVars item)) 
																	/= [] then outputTab ++ outputTab ++ errorLocationFormatting (srcSpan item)  
																	++ ": Non temporary, write variable (" ++ errorExprFormatting item ++ ") accessed without use of full loop variable\n"  else "") nonTempWrittenOperands

hasVarName :: [VarName [String]] -> Expr [String] -> Bool
hasVarName loopWrites (Var _ _ list) = foldl (\accum item -> if item then item else accum) False $ map (\(varname, exprs) -> elem varname loopWrites) list
hasVarName loopWrites _ = False

--	Used by analyseLoop_map to format the information on the position of a particular piece of code that is used as the information
--	output to the user
errorLocationFormatting :: SrcSpan -> String
errorLocationFormatting ((SrcLoc filename line column), srcEnd) = "line " ++ show line -- ++ ", column " ++ show column

errorExprFormatting :: Expr [String] -> String
errorExprFormatting (Var _ _ list) = foldl (++) "" (map (\(varname, exprList) -> ((\(VarName _ str) -> str) varname) ++ 
															(if exprList /= [] then "(" ++ (foldl (\accum item -> (if accum /= "" then accum ++ "," else "") 
																++ item) "" (map (errorExprFormatting) exprList)) ++ ")" else "")) list)
errorExprFormatting (Con _ _ str) = str
errorExprFormatting codeSeg = show codeSeg

--paralleliseProgram :: (Typeable p, Data p) => ProgUnit p -> ProgUnit p 
paralleliseProgram :: Program [String] -> Program [String] 
-- paralleliseProgram = map (gmapT (mkT (transformBlock))) 
paralleliseProgram codeSeg = map (everywhere (mkT (transformBlock (accessAnalysis)))) codeSeg
	where
		accessAnalysis = analyseAllVarAccess codeSeg

transformBlock :: VarAccessAnalysis -> Block [String] -> Block [String]
transformBlock accessAnalysis block = gmapT (mkT (transformForLoop accessAnalysis)) block
		--where 
		--	accessAnalysis = analyseAllVarAccess block

transformForLoop :: VarAccessAnalysis -> Fortran [String] -> Fortran [String]
transformForLoop  accessAnalysis inp = case inp of
		For _ _ _ _ _ _ _ -> paralleliseLoop [] accessAnalysis $ gmapT (mkT (transformForLoop accessAnalysis )) inp
		_ -> gmapT (mkT (transformForLoop accessAnalysis)) inp

--	Function uses a SYB query to get all of the loop condtions contained within a particular AST. loopCondtions_query traverses the AST
--	and calls getLoopConditions when a Fortran node is encountered.
loopCondtions_query :: (Typeable p, Data p) =>  Fortran p -> [(VarName p, Expr p, Expr p, Expr p)]
loopCondtions_query = everything (++) (mkQ [] getLoopConditions)

getLoopConditions :: (Typeable p, Data p) => Fortran p -> [(VarName p, Expr p, Expr p, Expr p)]
getLoopConditions codeSeg = case codeSeg of
		For _ _ var start end step _ -> [(var, start, end, step)]
		OpenCLMap _ _ _ _ loopVars _ -> loopVars
		_ -> []

--	Returns an AST representing a set of assignments that determine the values of the loop variables that are being parallelised for a
--	given global_id value.
flattenLoopConditions :: Maybe (VarName p) -> (VarName p) -> [(VarName p, Expr p, Expr p, Expr p)] -> Fortran p
flattenLoopConditions prev globalId ((var, start, end, step):[]) = Assg 
																		(tag globalId) 
																		generatedSrcSpan 
																		(Var (tag globalId) generatedSrcSpan [(var, [])])
																		(primitiveMod globalId end)
flattenLoopConditions prev globalId ((var, start, end, step):xs) = 	FSeq 
																	(tag globalId) 
																	generatedSrcSpan (
																		Assg 
																		(tag globalId) 
																		generatedSrcSpan (
																			Var (tag globalId) generatedSrcSpan [(var, [])])
																		(flattenCondition_div globalId prev (multiplyLoopConditions xs) -- DIVISOR
																			)
																		)
																	 (flattenLoopConditions (Just var) globalId xs) -- FSeq p SrcSpan (Fortran p) (Fortran p) 

--	Function returns an AST represnting a standard division that is performed to calculate loop variable values.
flattenCondition_div :: VarName p -> Maybe (VarName p) -> Expr p -> Expr p
flattenCondition_div globalId (Just prev) divisor = Bin 
														(tag globalId) 
														generatedSrcSpan 
														(Div (tag globalId)) (
															Bin 
																(tag globalId) 
																generatedSrcSpan 
																(Minus (tag globalId)) 
																(Var 
																	(tag globalId) 
																	generatedSrcSpan 
																	[(globalId, [])]) 
																(Var 
																	(tag globalId) 
																	generatedSrcSpan 
																	[(prev, [])]))
														divisor
flattenCondition_div globalId Nothing divisor = 	Bin 
														(tag globalId) 
														generatedSrcSpan 
														(Div (tag globalId)) 
														(Var 
															(tag globalId) 
															generatedSrcSpan 
															[(globalId, [])]) 
														divisor

--	Function returns an AST represnting a standard modulus calculation that is performed to calculate loop variable values.
flattenCondition_mod :: VarName p -> Maybe (VarName p) -> Expr p -> Expr p
flattenCondition_mod globalId (Just prev) divisor = Bin 
														(tag globalId) 
														generatedSrcSpan 
														(Div (tag globalId)) 
														(Var 
															(tag globalId) 
															generatedSrcSpan 
															[(globalId, [])]) 
														divisor 

--	Fortran does not have a modulus operator as standard. Therefore, this function returns an AST represnting a modulus calculation
--	that only uses primitive operators (+, -, *, /)
primitiveMod :: VarName p -> Expr p -> Expr p 
primitiveMod quotient divisor = Bin 
									(tag quotient) 
									generatedSrcSpan 
									(Minus (tag quotient)) 
									(Var 
										(tag quotient) 
										generatedSrcSpan 
										[(quotient, [])]) 
									(Bin 
										(tag quotient) 
										generatedSrcSpan 
										(Mul (tag quotient)) 
										(Bin 
											(tag quotient) 
											generatedSrcSpan 
											(Div (tag quotient)) 
											(Var 
												(tag quotient) 
												generatedSrcSpan 
												[(quotient, [])]) 
											divisor) 
										divisor)

-- 	Used by flattenLoopConditions to produce an expression that multiplies together the loop variable dimensions. 
--	This will likely be changed.
multiplyLoopConditions :: [(VarName p, Expr p, Expr p, Expr p)] -> Expr p
multiplyLoopConditions ((var, start, end, step):[]) = end
multiplyLoopConditions ((var, start, end, step):xs) = Bin (tag var) generatedSrcSpan (Mul (tag var)) end (multiplyLoopConditions xs)

--	Traverses the AST and prooduces a single string that contains all of the parallelising errors for this particular run of the compiler.
--	compileErrorListing traverses the AST and applies getErrors to Fortran nodes (as currently they are the only nodes that have
--	ever have annotations applied. The resulting string is then output to the user.
compileErrorListing :: Program [String] -> String
compileErrorListing codeSeg = everything (++) (mkQ [] getErrors) codeSeg

getErrors :: Fortran [String] -> String
getErrors codeSeg = case tag codeSeg of
	[] -> ""
	_ -> "Loop at " ++ (errorLocationFormatting (srcSpan codeSeg)) ++ " cannot be parallelised.\n" ++ (foldl (++) "" (tag codeSeg)) ++ "\n"

--getErrors (For tag srcspan _ _ _ _ _) = "For loop at " ++ (errorLocationFormatting srcspan) ++ " cannot be parallelised." ++ (foldl (++) "" tag) ++ "\n"
--getErrors codeSeg = "Loop at " ++ (errorLocationFormatting (srcSpan codeSeg)) ++ " cannot be parallelised." ++ (foldl (++) "" (tag codeSeg)) ++ "\n"

--	Returns a list of all of the names of variables that are used in a particular AST. getVarNames_query performs the traversal and applies
--	getVarNames at appropriate moments.
getVarNames_query :: (Typeable p, Data p) =>  Fortran p -> [VarName p]
getVarNames_query fortran = everything (++) (mkQ [] getVarNames) fortran

getVarNames :: (Typeable p, Data p) =>  VarName p -> [VarName p]
getVarNames expr = [expr]

--getVarNameExprList :: (Typeable p, Data p) =>  Expr p -> [(VarName p, [Expr p])]
--getVarNameExprList (Var _ _ lst) = lst
--getVarNameExprList _ = []

--	Generic function that takes two lists a and b and returns a +list c that is all of the elements of a that do not appear in b.
listSubtract :: Eq a => [a] -> [a] -> [a]
listSubtract a b = filter (\x -> notElem x b) a

--	Generic function that removes all duplicate elements from a list.
listRemoveDuplications :: Eq a => [a] -> [a]
listRemoveDuplications a = foldl (\accum item -> if notElem item accum then accum ++ [item] else accum) [] a

--extractOperands :: (Typeable p, Data p) => Expr p -> [(VarName p, [Expr p])]
--extractOperands (Bin _ _ _ expr1 expr2) = extractOperands expr1 ++ extractOperands expr2
--extractOperands expr 					= getVarNameExprList expr

--extractExprs :: (Typeable p, Data p) => Expr p -> Expr p

--exprListContainsVarNames accesses loopVars

--	Function checks whether every Expr in a list is a VarName from another list.
exprListContainsVarNames :: (Typeable p, Data p, Eq p) =>  [Expr p] -> [VarName p] -> Bool
exprListContainsVarNames contains container = all (== True) (everything (++) (mkQ [] (varNameCheck container)) contains)

varNameCheck :: (Typeable p, Data p, Eq p) => [VarName p] -> VarName p -> [Bool]
varNameCheck container contains = [elem contains container]

--	Function checks whether every VarName in a list appears at least once in a list of Expr
exprListContainsAllVarNames :: (Typeable p, Data p, Eq p) => [VarName p] -> [Expr p] -> Bool
exprListContainsAllVarNames contains container = (foldl delete_foldl (listRemoveDuplications contains) (everything (++) (mkQ [] getVarNames) container)) == []

delete_foldl :: (Typeable p, Data p, Eq p) => [VarName p] -> VarName p -> [VarName p]
delete_foldl accum item = delete item accum

--	Function returns a boolean that shows whether or not a list of expressions contains any constants
constantCheck_query :: (Typeable p, Data p) => [Expr p] -> Bool
constantCheck_query exprList = all (== False) (everything (++) (mkQ [] (constantCheck)) exprList)

constantCheck :: Expr [String] -> [Bool]
constantCheck (Con _ _ _) = [True]
constantCheck _ = [False]

-- 	Function returns the loop variable for an AST representing a for loop
getLoopVar :: Fortran p -> Maybe(VarName p)
getLoopVar (For _ _ var _ _ _ _) = Just var
getLoopVar _ = Nothing

--	Value used as a global spacing measure. Used for output formatting.
outputTab :: String
outputTab = "  "


-- FUNCTIONS FOR DEBUGGING AND DEVELOPMENT

identifyLoops :: (Typeable p, Data p) => ProgUnit p -> [Fortran [String]]
identifyLoops program =
	everything
		(++)
		(mkQ [] checkLoop)
		program

checkLoop inp = case inp of
		For _ _ _ _ _ _ _ -> [inp]
		_ -> []

contains_list :: [Variable] -> [Variable] -> Bool
contains_list container contained = all (== True) (Prelude.map (\x -> elem x container) contained)

contains_list_rejectEmpty :: [Variable] -> [Variable] -> Bool
contains_list_rejectEmpty container [] = False
contains_list_rejectEmpty container contained = all (== True) (Prelude.map (\x -> elem x container) contained)

arbitraryChange_allChildren :: (Data a, Typeable a) => String -> Fortran a -> Fortran a
arbitraryChange_allChildren comment = everywhere (mkT (modifySrcSpan_allChildren comment)) 

modifySrcSpan_allChildren :: String -> SrcSpan -> SrcSpan
modifySrcSpan_allChildren comment (a, b) = (SrcLoc {srcFilename = comment, srcLine = 10, srcColumn = -1}, b)

test_exprListContainsAllVarNames :: (Typeable p, Data p, Eq p) =>  [Expr p] -> [VarName p] -> [VarName p]
test_exprListContainsAllVarNames container contains = (foldl delete_foldl (listRemoveDuplications contains) (everything (++) (mkQ [] getVarNames) container))