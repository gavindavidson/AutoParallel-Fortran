module Main where

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List
import PreProcessor
import System.Environment

--	Type used to standardise loop analysis functions
--						errors 		reduction variables read variables		written variables		
type AnalysisInfo = 	(String, 	[Expr [String]], 	[Expr [String]], 	[Expr [String]])
--	Type used to colate data on variable accesses throughout a program.
--							Name of variable 	All reads 	All writes
type VarAccessAnalysis = 	[(VarName [String], [SrcSpan], 	[SrcSpan])]

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

	--putStr $ show $ parsedProgram
	--putStr "\n\n\n"

	--putStr $ show $ parallelisedProg
	--putStr "\n"

	--putStr "\n"

--	Taken from language-fortran example. Runs preprocessor on target source and then parses the result, returning an AST.
parseFile s = do inp <- readFile s
                 return $ parse $ preProcess inp

--	Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new parallel (OpenCLMap etc)
--	nodes or the original sub-tree annotated with parallelisation errors.
paralleliseLoop :: [VarName [String]] -> VarAccessAnalysis ->Fortran [String] -> Fortran [String]
paralleliseLoop loopVars accessAnalysis loop 	=	case mapAttempt_bool of
										True	-> mapAttempt_ast
										False 	-> case reduceAttempt_bool of
													True 	-> reduceAttempt_ast
													False	-> reduceAttempt_ast
								--case paralleliseLoop_map loop newLoopVars of 
								--	Just a -> a
								--	Nothing -> loop
								where
									newLoopVars = case getLoopVar loop of
										Just a -> loopVars ++ [a]
										Nothing -> loopVars

									tempVars = []
									nonTempVars = getNonTempVars (srcSpan loop) accessAnalysis

									loopWrites = extractWrites_query loop

									mapAttempt = paralleliseLoop_map loop newLoopVars loopWrites tempVars
									mapAttempt_bool = fst mapAttempt
									mapAttempt_ast = snd mapAttempt

									reduceAttempt = paralleliseLoop_reduce mapAttempt_ast newLoopVars loopWrites tempVars
									reduceAttempt_bool = fst reduceAttempt
									reduceAttempt_ast = snd reduceAttempt

extractWrites_query :: (Typeable p, Data p) => Fortran p -> [VarName p]
extractWrites_query = everything (++) (mkQ [] extractWrites)

extractWrites :: (Typeable p, Data p) => Fortran p -> [VarName p]
extractWrites (Assg _ _ (Var _ _ list) _) = map (\(varname, exprs) -> varname) list
extractWrites _ = []

--	Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new OpenCLMap nodes or the
--	original sub-tree annotated with reasons why the loop cannot be mapped
paralleliseLoop_map :: Fortran [String] -> [VarName [String]] -> [VarName [String]] -> [VarName [String]] -> (Bool, Fortran [String])
paralleliseLoop_map loop loopVars loopWrites tempVars 	|	errors_map == "" 	=	(True,
																	OpenCLMap [] generatedSrcSpan 
													 				--(listRemoveDuplications (listSubtract (getVarNames_query loop) (Prelude.map (\(a, _, _, _) -> a) (loopCondtions_query loop))) ) 
													 				(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames reads_map)) (map (\(a, _, _, _) -> a) (loopCondtions_query loop)))
													 				(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames writes_map)) (map (\(a, _, _, _) -> a) (loopCondtions_query loop)))
																	--(flattenLoopConditions Nothing (VarName [] "g_id") 
																	(loopCondtions_query loop) --)   
																	--(removeLoopConstructs_trans loop))
																	(removeLoopConstructs_recursive loop))
									|	otherwise	=			(False, addAnnotation loop (outputTab ++ "Cannot map due to:\n" ++ errors_map))
									where
										(errors_map, _, reads_map, writes_map) = analyseLoop_map loopVars loopWrites tempVars loop

--	Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new OpenCLReduce nodes or the
--	original sub-tree annotated with reasons why the loop is not a reduction
paralleliseLoop_reduce ::Fortran [String] -> [VarName [String]] -> [VarName [String]] -> [VarName [String]] -> (Bool, Fortran [String])
paralleliseLoop_reduce loop loopVars loopWrites tempVars	|	errors_reduce == "" 	=	(True, addAnnotation loop (outputTab ++ "REDUCTION POSSIBLE:\n"))
										|	otherwise	=				(False, addAnnotation loop (outputTab ++ "Cannot reduce due to:\n" ++ errors_reduce))
									where
										(errors_reduce, reducionVariable, reads_reduce, writes_reduce) = analyseLoop_reduce [] loopVars loopWrites tempVars loop 

--	Function takes a list of loop variables and a possible parallel loop's AST and returns a string that details the reasons why the loop
--	cannot be mapped. If the returned string is empty, the loop represents a possible parallel map
analyseLoop_map :: [VarName [String]] -> [VarName [String]] -> [VarName [String]] -> Fortran [String] -> AnalysisInfo
analyseLoop_map loopVars loopWrites tempVars codeSeg = case codeSeg of
		Assg _ srcspan expr1 expr2 -> combineAnalysisInfo (combineAnalysisInfo (analyseAccess_map loopVars loopWrites tempVars expr1) (analyseAccess_map loopVars loopWrites tempVars expr2)) ("",[],[],[expr1])
		For _ _ var _ _ _ _ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_map (loopVars ++ [var]) loopWrites tempVars)) codeSeg)
		_ -> foldl combineAnalysisInfo analysisInfoBaseCase ((gmapQ (mkQ analysisInfoBaseCase (analyseLoop_map loopVars loopWrites tempVars)) codeSeg) ++ (gmapQ (mkQ analysisInfoBaseCase (analyseAccess_map loopVars loopWrites tempVars)) codeSeg))


--analyseLoop_map loopVars loopWrites codeSeg = case codeSeg of
--		Assg _ srcspan expr1 expr2 ->	(
--										(if assignments == [] then 
--												outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": assignment to scalar variable\n" else "")
										
--										-- ++ (if not (constantCheck_query assignments) then 
--										--		outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": assignment to constant array element\n" else "") ++

--										++ (if not (all (== True) (map (exprListContainsAllVarNames loopVars) assignments)) then 
--												outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": assignment to array element with non-loop variable dimensions\n" else "")

--										++ (if (not (all (== True) (map (exprListContainsAllVarNames loopVars) (filter (\x -> x /= []) accesses) ))) then 
--												outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": access to array element with non-loop variable dimensions\n" else "")
--										,
--										extractOperands expr2,
--										[expr1])
--										-- ++ "ASSIGNMENTS: " ++ show assignments 
--										-- ++ "\nACCESSES: " ++ show accesses ++ "\n"
--										-- ++ (if not (exprListContainsVarNames assignments loopVars) then 
--										--		outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": assignment to array element at non-loop variable position\n" else "") ++
										
--										-- ++ (if not (exprListContainsVarNames accesses loopVars) then 
--										--		outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": access to an array element not related to the loop current loop variable scope\n" else "")
--		--(assignments /= [])	&& (exprListContainsVarNames assignments loopVars) 	&& (constantCheck_query assignments) 
--		--											&& (exprListContainsVarNames accesses loopVars) 	-- && (constantCheck_query accesses)
--			where
--				assignments = arrayAccesses_query expr1
--				accesses = arrayAccesses_query expr2
--		For _ _ var _ _ _ _ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_map (loopVars ++ [var]) loopWrites)) codeSeg)
--		_ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_map loopVars loopWrites)) codeSeg)

--analyseLoop_reduce :: [VarName [String]] -> Fortran [String] -> (String, [Expr [String]], [Expr [String]])
analyseLoop_reduce :: [Expr [String]] -> [VarName [String]] -> [VarName [String]] -> [VarName [String]] -> Fortran [String] -> AnalysisInfo
analyseLoop_reduce condExprs loopVars loopWrites tempVars codeSeg = case codeSeg of
		Assg _ srcspan expr1 expr2 -> 	(
										(if not potentialReductionVar then 
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
				nonTempOperands = filter (\x -> not $ hasVarName tempVars x) operands
				tempAssignment = nonTempOperands == []
				potentialReductionVar = not tempAssignment && ( (hasOperand expr2 expr1) || (foldl (\accum item -> if item then item else accum) False $ map (\x -> hasOperand x expr1) condExprs) ) 
		If _ _ expr _ _ _ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce (condExprs ++ [expr]) loopVars loopWrites tempVars)) codeSeg)
		For _ _ var _ _ _ _ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce condExprs (loopVars ++ [var]) loopWrites tempVars)) codeSeg)
		_ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce condExprs loopVars loopWrites tempVars)) codeSeg)

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

analyseAccess_map :: [VarName [String]] -> [VarName [String]] -> [VarName [String]] -> Expr [String] -> AnalysisInfo
analyseAccess_map loopVars loopWrites tempVars expr = (errors, [],[expr],[])
								where
									operands = extractOperands expr
									writtenOperands = filter (hasVarName loopWrites) operands
									nonTempWrittenOperands = filter (\x -> not $ hasVarName tempVars x) writtenOperands
									errors = foldl (++) "" $ map (\item -> 

																if listSubtract loopVars (foldl (\accum item -> accum ++ extractVarNames item) [] (extractArrayAccesses item)) 
																	/= [] then outputTab ++ outputTab ++ errorLocationFormatting (srcSpan item)  
																	++ ": Non read only (" ++ errorExprFormatting item ++ ") accessed without use of full loop variable\n"  else "") nonTempWrittenOperands

analyseAllVarAccess:: Block [String] -> VarAccessAnalysis
analyseAllVarAccess block = foldl (combineVarAccessAnalysis) [] (gmapQ (mkQ [] (analyseAllVarAccess_fortran [])) block)

analyseAllVarAccess_fortran :: VarAccessAnalysis -> Fortran [String] -> VarAccessAnalysis
analyseAllVarAccess_fortran prevAnalysis (Assg _ _ writeExpr readExpr) = combineVarAccessAnalysis prevAnalysis analysisWithWritesReads
												where
													readVarNames = foldl (\accum item -> accum ++ (extractVarNames item)) [] (extractOperands readExpr)
													writtenVarNames = foldl (\accum item -> accum ++ (extractVarNames item)) [] (extractOperands writeExpr)
													analysisWithReads = foldl (addVarReadAccess (srcSpan readExpr)) prevAnalysis readVarNames
													analysisWithWritesReads = foldl (addVarWriteAccess (srcSpan readExpr)) analysisWithReads writtenVarNames

analyseAllVarAccess_fortran prevAnalysis codeSeg =  foldl (combineVarAccessAnalysis) [] (gmapQ (mkQ [] (analyseAllVarAccess_fortran currentAnalysis)) codeSeg)
												where 
													extractedExprs = gmapQ (mkQ (Null [] generatedSrcSpan) extractExprs) codeSeg
													extractedOperands = foldl (\accum item -> accum ++ extractOperands item) [] extractedExprs
													readVarNames = foldl (\accum item -> accum ++ extractVarNames item) [] extractedOperands	
													analysisWithReads = foldl (addVarReadAccess (srcSpan codeSeg)) prevAnalysis readVarNames	
													currentAnalysis = combineVarAccessAnalysis prevAnalysis analysisWithReads							

addVarReadAccess :: SrcSpan -> VarAccessAnalysis -> VarName [String] -> VarAccessAnalysis
addVarReadAccess srcspan ((varnameAnalysis, src_reads, src_writes):xs) varname  | varnameAnalysis == varname = [(varname, src_reads ++ [srcspan], src_writes)] ++ xs
																				| otherwise = [(varnameAnalysis, src_reads, src_writes)] ++ (addVarReadAccess srcspan xs varname)
addVarReadAccess srcspan [] varname	= [(varname, [srcspan], [])]

addVarWriteAccess :: SrcSpan -> VarAccessAnalysis -> VarName [String] -> VarAccessAnalysis
addVarWriteAccess srcspan ((varnameAnalysis, src_reads, src_writes):xs) varname  | varnameAnalysis == varname = [(varname, src_reads, src_writes ++ [srcspan])] ++ xs
																				| otherwise = [(varnameAnalysis, src_reads, src_writes)] ++ (addVarWriteAccess srcspan xs varname)
addVarWriteAccess srcspan [] varname	= [(varname, [], [srcspan])]															

combineVarAccessAnalysis :: VarAccessAnalysis -> VarAccessAnalysis -> VarAccessAnalysis
combineVarAccessAnalysis a b = foldl (addVarAccessAnalysis) a b 

addVarAccessAnalysis :: VarAccessAnalysis -> (VarName [String], [SrcSpan], 	[SrcSpan]) -> VarAccessAnalysis
addVarAccessAnalysis ((varnameAnalysis, readsAnalysis, writesAnalysis):xs) (newVarName, newReads, newWrites) 	| varnameAnalysis == newVarName = [(varnameAnalysis, readsAnalysis ++ newReads, writesAnalysis ++ newWrites)] ++ xs
																												| otherwise = [(varnameAnalysis, readsAnalysis, writesAnalysis)] ++ (addVarAccessAnalysis xs (newVarName, newReads, newWrites))
addVarAccessAnalysis [] (newVarName, newReads, newWrites) = [(newVarName, newReads, newWrites)]

getNonTempVars :: SrcSpan -> VarAccessAnalysis -> [VarName [String]]
getNonTempVars codeBlockSpan accessAnalysis = map (\(x, _, _) -> x) hangingReads
						where
							readsAfterBlock = varAccessAnalysis_readsAfter codeBlockSpan accessAnalysis
							writesReadsAfterBlock = varAccessAnalysis_writesAfter codeBlockSpan readsAfterBlock
							hangingReads = filter (checkHangingReads) writesReadsAfterBlock


varAccessAnalysis_writesAfter :: SrcSpan -> VarAccessAnalysis -> VarAccessAnalysis
varAccessAnalysis_writesAfter codeBlockSpan accessAnalysis = foldl (\accum item -> accum ++ varAccessAnalysis_writesAfter' codeBlockSpan item) [] accessAnalysis

varAccessAnalysis_writesAfter' :: SrcSpan -> (VarName [String], [SrcSpan], [SrcSpan]) ->  VarAccessAnalysis
varAccessAnalysis_writesAfter' (start, SrcLoc file_end line_end column_end) (varname, readSpans, writeSpans) = [(varname, readSpans, newWriteSpans)]
										where
											newWriteSpans = filter (\((SrcLoc _ line_write column_write), _) -> line_write >= line_end) writeSpans

varAccessAnalysis_readsAfter :: SrcSpan -> VarAccessAnalysis -> VarAccessAnalysis
varAccessAnalysis_readsAfter codeBlockSpan accessAnalysis = foldl (\accum item -> accum ++ varAccessAnalysis_readsAfter' codeBlockSpan item) [] accessAnalysis

varAccessAnalysis_readsAfter' :: SrcSpan -> (VarName [String], [SrcSpan], [SrcSpan]) ->  VarAccessAnalysis
varAccessAnalysis_readsAfter' (start, SrcLoc file_end line_end column_end) (varname, readSpans, writeSpans) = if newReadSpans /= [] then [(varname, newReadSpans, writeSpans)] else []
										where
											newReadSpans = filter (\((SrcLoc _ line_read column_read), _) -> line_read >= line_end) readSpans

checkHangingReads :: (VarName [String], [SrcSpan], [SrcSpan]) -> Bool
checkHangingReads (varname, readSpans, writeSpans) = case earliestRead of
														Just r ->	case earliestWrite of
																		Just w -> checkSrcSpanBefore r w
																		Nothing -> True
														Nothing ->	False
								where 
									earliestRead = getEarliestSrcSpan readSpans
									earliestWrite = getEarliestSrcSpan writeSpans

getEarliestSrcSpan :: [SrcSpan] -> Maybe(SrcSpan)
getEarliestSrcSpan [] = Nothing
getEarliestSrcSpan spans = Just (foldl (\accum item -> if checkSrcSpanBefore item accum then item else accum) (spans!!0) spans)

checkSrcSpanBefore :: SrcSpan -> SrcSpan -> Bool
checkSrcSpanBefore ((SrcLoc file_before line_before column_before), beforeEnd) ((SrcLoc file_after line_after column_after), afterEnd) = (line_before < line_after) && (column_before < column_after)

--	This function takes two nodes where one is parent of the other. It producces a list of VarNames that represent variables that are temporary within the
--	non-parent node. For example, the nodes may be a For loop that is contained within an FSeq. The function returns a list of VarNames that appear in the
--	loop whose values are not read after the loop without being previously reassigned. This is done by finding a list of reads (VarNames) in the parent 
--	node (not including the child's children) whose values do not come from code within the node and matching it against a list of writes in the child node.
--	The child's writes that do not match any of the parent's reads are temporary variables within the child node. 	
--getTempVariables :: Fortran[String] -> Fortran [String] -> [VarName [String]]
--getTempVariables codeSegParent codeSeg = listSubtract loopWrites followingReads
--									where
--										followingReads = foldl (++) [] $ gmapQ (mkQ [] $ extractUnassignedReads_skip codeSeg) codeSegParent
--										loopWrites = extractWrites_query codeSeg


--matchTest :: Fortran[String] -> Fortran [String] -> String
--matchTest codeSegParent codeSeg = show (gmapQ (mkQ [] $ skip codeSeg) codeSegParent) ++"\nPARENT: " ++ show (codeSegParent) ++ "\nCHILD: " ++ show(codeSeg) ++ "\n\n"

----show $ gmapQ (mkQ [] $ skip codeSeg) codeSegParent

--skip :: Fortran[String] -> Fortran[String] -> [Bool]
--skip codeSegSkip codeSeg 	|	codeSegSkip == codeSeg = [False]
--							|	otherwise = [True]

----	Function is used to process the parent node (see getTempVariables) without processing the child node in question.
--extractUnassignedReads_skip :: Fortran[String] -> Fortran[String] -> [VarName [String]]
--extractUnassignedReads_skip codeSegSkip codeSeg |	codeSegSkip == codeSeg = []
--												|	otherwise = fst (extractUnassignedReads ([],[]) codeSeg )

----extractUnassignedReads_skip codeSegSkip codeSeg = fst (extractUnassignedReads ([],[]) codeSeg )

----	This recursive function takes a node returns a tuple containing a list of VarNames that are written to and a list of VarNames that are read without being written
----	to previously within that node. The first element of the returned tuple is the important one for the overall functionality as it represents the variables
----	whose values are not accounted for in this node, as in they receive their value from elsewhere.
--extractUnassignedReads :: ([VarName [String]], [VarName [String]]) -> Fortran[String] -> ([VarName [String]], [VarName [String]])
--extractUnassignedReads (prevRead,prevWritten)  (Assg _ _ writeExpr readExpr) = (newRead, newWritten)
--							where 
--								writtenVars = foldl (\accum item -> accum ++ extractVarNames item) [] (extractOperands writeExpr)
--								readVars = foldl (\accum item -> accum ++ extractVarNames item) [] (extractOperands readExpr)
--								newWritten = foldl (\accum item -> if not $ elem item accum then accum ++ [item] else accum) prevWritten writtenVars
--								newRead = filter (\item -> not $ elem item newWritten) readVars
--extractUnassignedReads (prevRead, prevWritten) codeSeg = (newRead ++ nextResultRead, prevWritten)
--							where
--								extractedExprs = gmapQ (mkQ (Null [] generatedSrcSpan) extractExprs) codeSeg
--								extractedOperands = foldl (\accum item -> accum ++ extractOperands item) [] extractedExprs
--								readVars = listRemoveDuplications $ foldl (\accum item -> accum ++ extractVarNames item) [] extractedOperands
--								newRead = (filter (\item -> not $ elem item prevWritten) readVars) ++ prevRead

--								(nextResultRead, nextResultWritten) = foldl (\(accumR, accumW) (itemR, itemW) -> (accumR ++ itemR, accumW ++ itemW)) ([], []) (gmapQ (mkQ ([],[]) (extractUnassignedReads (newRead,prevWritten))) codeSeg)
								-- foldl (\accum item -> accum ++ extractVarNames item) [] (extractOperands readExpr)

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

--	Top level function that is called on the AST of a program. Function traverses the program unit and applies transformForLoop when a
--	Fortran node is encountered. The function performs a transformation of the input AST and so returns a version whose loops are either
--	annotated or made parallel
--paralleliseProgram :: (Typeable p, Data p) => ProgUnit p -> ProgUnit p 
--paralleliseProgram = everywhere (mkT (transformForLoop))

--	When a Fortran node is encountered by paralleliseProgram, this function is used. It's functionality is to differentiate between nodes
--	that represent for loops and those that do not. When a for loop is encountered, it is attempted to be parallelised using the 
-- 	paralleliseLoop function
--transformForLoop :: Fortran [String] -> Fortran [String]
--transformForLoop inp = case inp of
--		For _ _ _ _ _ _ _ -> paralleliseLoop [] inp
--		_ -> inp

--paralleliseProgram :: (Typeable p, Data p) => ProgUnit p -> ProgUnit p 
paralleliseProgram :: (Typeable p, Data p) => Program p -> Program p 
paralleliseProgram = map (gmapT (mkT (transformBlock))) 

transformBlock :: Block [String] -> Block [String]
transformBlock block = gmapT (mkT (transformForLoop accessAnalysis)) block
		where 
			accessAnalysis = analyseAllVarAccess block

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

--	Takes an AST and removes the loop statements from the node and joins up the rest of the code so that is it represented in the
--	format that language-fortran uses.
removeLoopConstructs_recursive :: Fortran [String] -> Fortran [String]
removeLoopConstructs_recursive (FSeq _ _ (For _ _ _ _ _ _ (FSeq anno srcspan fortran11 fortran12)) fortran02) = FSeq anno srcspan fortran11 
																															(appendFortran_recursive 	(removeLoopConstructs_recursive fortran02) 
																																						(removeLoopConstructs_recursive fortran12))
removeLoopConstructs_recursive (FSeq _ _ (OpenCLMap _ _ _ _ _ (FSeq anno srcspan fortran11 fortran12)) fortran02) = FSeq anno srcspan fortran11 
																															(appendFortran_recursive 	(removeLoopConstructs_recursive fortran02) 
																																						(removeLoopConstructs_recursive fortran12))
removeLoopConstructs_recursive (For _ _ _ _ _ _ fortran1) = removeLoopConstructs_recursive fortran1
removeLoopConstructs_recursive (OpenCLMap _ _ _ _ _ fortran1) = removeLoopConstructs_recursive fortran1
removeLoopConstructs_recursive (NullStmt anno srcspan) = NullStmt anno srcspan
removeLoopConstructs_recursive codeSeg = gmapT (mkT removeLoopConstructs_recursive) codeSeg

--	Takes two ASTs and appends on onto the other so that the resulting AST is in the correct format
appendFortran_recursive :: Fortran [String] -> Fortran [String] -> Fortran [String]
appendFortran_recursive newFortran (FSeq _ _ _ (FSeq _ _ _ fortran1)) = appendFortran_recursive newFortran fortran1 
appendFortran_recursive newFortran (FSeq _ _ fortran1 fortran2) = FSeq [] generatedSrcSpan fortran1 (FSeq [] generatedSrcSpan fortran2 newFortran)
appendFortran_recursive newFortran codeSeg = FSeq [] generatedSrcSpan codeSeg newFortran

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
extractExprs :: Expr [String] -> Expr [String] 
extractExprs expr = expr

extractOperands :: (Typeable p, Data p) => Expr p -> [Expr p]
extractOperands (Bin _ _ _ expr1 expr2) = extractOperands expr1 ++ extractOperands expr2
extractOperands expr 					= [expr]

extractVarNames :: (Typeable p, Data p) => Expr p -> [VarName p]
extractVarNames (Var _ _ lst) = map (\(x, _) -> x) lst
extractVarNames _ = []

extractArrayAccesses :: (Typeable p, Data p) => Expr p -> [Expr p]
extractArrayAccesses (Var _ _ lst) = foldl (\accumExprs (itemVar, itemExprs) -> accumExprs ++ itemExprs) [] lst
extractArrayAccesses _ = []

--	Function takes an AST reprenting an expression and returns a nested list representing the elements that are accessed by each term
-- 	of the original expression
--arrayAccesses_query :: (Typeable p, Data p) =>  Expr p -> [[Expr p]]
--arrayAccesses_query codeSeg = case codeSeg of
--	Var _ _ lst -> [everything (++) (mkQ [] getArrayAccesses) codeSeg]
--	Bin _ _ _ expr1 expr2 -> arrayAccesses_query expr1 ++ arrayAccesses_query expr2
--	_ -> []

--getArrayAccesses :: (Typeable p, Data p) => Expr p -> [Expr p]
--getArrayAccesses codeSeg = case codeSeg of
--	Var _ _ lst -> foldl concatExprList_foldl [] lst
--	--Bin _ _ _ expr1 expr2 -> arrayAccesses_query expr1 ++ arrayAccesses_query expr2
--	_ -> []

-- (gmapQ (mkQ "" (analyseLoop_map loopVars)) codeSeg)

--arrayAccesses_query :: (Typeable p, Data p) =>  Expr p -> [[Expr p]]
--arrayAccesses_query = everything (++) (mkQ [] getArrayAccesses)

--getArrayAccesses :: (Typeable p, Data p) => Expr p -> [[Expr p]]
--getArrayAccesses codeSeg = case codeSeg of
--	Var _ _ lst -> [foldl concatExprList_foldl [] lst]
--	Bin _ _ _ expr1 expr2 -> arrayAccesses_query expr1 ++ arrayAccesses_query expr2
--	_ -> []

--	Used as part of a foldl to concatentate expressions taken from 
concatExprList_foldl :: (Typeable p, Data p) => [Expr p] -> (VarName p, [Expr p]) -> [Expr p]
concatExprList_foldl prev (var, exprs) = prev ++ exprs

--exprListContainsVarNames accesses loopVars

--	Function checks whether every Expr in a list is a VarName from another list.
exprListContainsVarNames :: (Typeable p, Data p, Eq p) =>  [Expr p] -> [VarName p] -> Bool
exprListContainsVarNames contains container = all (== True) (everything (++) (mkQ [] (varNameCheck container)) contains)

varNameCheck :: (Typeable p, Data p, Eq p) => [VarName p] -> VarName p -> [Bool]
varNameCheck container contains = [elem contains container]

--	Function checks whether every VarName in a list appears at least once in a list of Expr
exprListContainsAllVarNames :: (Typeable p, Data p, Eq p) => [VarName p] -> [Expr p] -> Bool
exprListContainsAllVarNames contains container = (foldl delete_foldl (listRemoveDuplications contains) (everything (++) (mkQ [] getVarNames) container)) == []

exprListContainsAllVarNames_debug :: (Typeable p, Data p, Eq p) => [VarName p] -> [Expr p] -> [VarName p]
exprListContainsAllVarNames_debug contains container = (everything (++) (mkQ [] getVarNames) container)

delete_foldl :: (Typeable p, Data p, Eq p) => [VarName p] -> VarName p -> [VarName p]
delete_foldl accum item = delete item accum

--exprListContainsAllVarNames :: (Typeable p, Data p, Eq p) =>  [Expr p] -> [VarName p] -> Bool
--exprListContainsAllVarNames container contains = (foldl delete_foldl (listRemoveDuplications contains) (everything (++) (mkQ [] getVarNames) container)) == []

	--all (== True) (everything (++) (mkQ [] (allVarNameCheck container)) contains)

--	Function returns a boolean that shows whether or not a list of expressions contains any constants
constantCheck_query :: (Typeable p, Data p) => [Expr p] -> Bool
constantCheck_query exprList = all (== False) (everything (++) (mkQ [] (constantCheck)) exprList)

constantCheck :: Expr [String] -> [Bool]
constantCheck (Con _ _ _) = [True]
constantCheck _ = [False]

--accessVarCheck_query :: (Typeable p, Data p, Eq p) => [VarName p] -> [[Expr p]] -> Bool
--accessVarCheck_query loopVars exprList = all (== True) (everything (++) (mkQ [] (accessVarCheck loopVars)) exprList)

--accessVarCheck :: (Typeable p, Data p, Eq p) => [VarName p] -> VarName p -> [Bool]
--accessVarCheck loopVars varname = [elem varname loopVars]

-- 	Function returns the loop variable for an AST representing a for loop
getLoopVar :: Fortran p -> Maybe(VarName p)
getLoopVar (For _ _ var _ _ _ _) = Just var
getLoopVar _ = Nothing

--	Generates a SrcSpan that is attached to nodes that have been generated by this program
generatedSrcSpan :: SrcSpan
generatedSrcSpan = (SrcLoc {srcFilename = "GENERATED", srcLine = -1, srcColumn = -1}, SrcLoc {srcFilename = "GENERATED", srcLine = -1, srcColumn = -1})

standardiseSrcSpan_trans ::(Data a, Typeable a) =>  Expr a -> Expr a
standardiseSrcSpan_trans = everywhere (mkT (standardiseSrcSpan))

standardiseSrcSpan :: SrcSpan -> SrcSpan
standardiseSrcSpan src = generatedSrcSpan

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