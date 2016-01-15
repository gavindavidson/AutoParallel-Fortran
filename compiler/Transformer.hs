module Main where

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List
import PreProcessor
import System.Environment

main :: IO ()
main = do
	--a <- parseFile "../testFiles/arrayLoop.f95"
	
	args <- getArgs
	let filename = args!!0
	--a <- parseFile "../testFiles/arrayLoop.f95"
	parsedProgram <- parseFile filename
	let parallelisedProg = paralleliseProgram (parsedProgram!!0)

	putStr $ compileErrorListing parallelisedProg
	putStr "\n\n\n"

	putStr $ show $ (parsedProgram!!0)
	putStr "\n\n\n"

	putStr $ show $ parallelisedProg
	putStr "\n"

	--putStr "\n"

--	Taken from language-fortran example. Runs preprocessor on target source and then parses the result, returning an AST.
parseFile s = do delete_foldl <- readFile s
                 return $ parse $ preProcess delete_foldl

--	Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new parallel (OpenCLMap etc)
--	nodes or the original sub-tree annotated with parallelisation errors.
paralleliseLoop :: [VarName [String]] -> Fortran [String] -> Fortran [String]
paralleliseLoop loopVars loop 	=	case mapAttempt_bool of
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

									mapAttempt = paralleliseLoop_map loop newLoopVars
									mapAttempt_bool = fst mapAttempt
									mapAttempt_ast = snd mapAttempt

									reduceAttempt = paralleliseLoop_reduce mapAttempt_ast newLoopVars
									reduceAttempt_bool = fst reduceAttempt
									reduceAttempt_ast = snd reduceAttempt

--	Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new OpenCLMap nodes or the
--	original sub-tree annotated with reasons why the loop cannot be mapped
paralleliseLoop_map :: Fortran [String] -> [VarName [String]] -> (Bool, Fortran [String])
paralleliseLoop_map loop loopVars 	|	errors_map == "" 	=	(True,
																	OpenCLMap [] generatedSrcSpan 
													 				(listRemoveDuplications (listSubtract (getVarNames_query loop) (Prelude.map (\(a, _, _, _) -> a) (loopCondtions_query loop))) ) 
																	--(flattenLoopConditions Nothing (VarName [] "g_id") 
																	(loopCondtions_query loop) --)   
																	--(removeLoopConstructs_trans loop))
																	(removeLoopConstructs_recursive loop))
									|	otherwise	=			(False, addAnnotation loop (outputTab ++ "Cannot map due to:\n" ++ errors_map))
									where
										errors_map = getErrors_map loopVars loop

--paralleliseLoop_map loop loopVars	|	checkAssignments_map loopVars loop = Just (OpenCLMap [] generatedSrcSpan 
--																	 				(listRemoveDuplications (listSubtract (getVarNames_query loop) (Prelude.map (\(a, _, _, _) -> a) (loopCondtions_query loop))) ) 
--																					(flattenLoopConditions Nothing (VarName [] "g_id") (loopCondtions_query loop))   
--																					(removeLoopConstructs_trans loop)) -- Just (arbitraryChange_allChildren "PARALLEL" loop)
--									|	otherwise		= Just (addAnnotation loop ("  Cannot map due to:\n" ++ (getErrors_map loopVars loop)))
									
									-- |	otherwise		= Nothing

--checkAssignments_map :: [VarName [String]] -> Fortran [String] -> Bool
--checkAssignments_map loopVars codeSeg = case codeSeg of
--		Assg _ srcspan expr1 expr2 -> 		(assignments /= [])	
--										&& 	(exprListContainsVarNames assignments loopVars) 	
--										&& 	(constantCheck_query assignments) 
--										&& 	(exprListContainsVarNames accesses loopVars) 	-- && (constantCheck_query accesses)
--			where
--				assignments = arrayAccesses_query expr1
--				accesses = arrayAccesses_query expr2
--		For _ _ var _ _ _ _ -> all (== True) (gmapQ (mkQ True (checkAssignments_map (loopVars ++ [var]) )) codeSeg)
--		_ -> all (== True) (gmapQ (mkQ True (checkAssignments_map loopVars)) codeSeg)

--	Function takes a list of loop variables and a possible parallel loop's AST and returns a string that details the reasons why the loop
--	cannot be mapped. If the returned string is empty, the loop represents a possible parallel map
getErrors_map :: [VarName [String]] -> Fortran [String] -> String
getErrors_map loopVars codeSeg = case codeSeg of
		Assg _ srcspan expr1 expr2 ->	(if assignments == [] then 
												outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": assignment to scalar variable\n" else "")
										
										-- ++ (if not (constantCheck_query assignments) then 
										--		outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": assignment to constant array element\n" else "") ++
										
										++ (if not (all (== True) (map (exprListContainsAllVarNames loopVars) assignments)) then 
												outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": assignment to array element with non-loop variable dimensions\n" else "")

										++ (if (not (all (== True) (map (exprListContainsAllVarNames loopVars) accesses))) && accesses /= [] then 
												outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": access to array element with non-loop variable dimensions\n" else "")

										-- ++ (if not (exprListContainsVarNames assignments loopVars) then 
										--		outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": assignment to array element at non-loop variable position\n" else "") ++
										
										-- ++ (if not (exprListContainsVarNames accesses loopVars) then 
										--		outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ": access to an array element not related to the loop current loop variable scope\n" else "")
		--(assignments /= [])	&& (exprListContainsVarNames assignments loopVars) 	&& (constantCheck_query assignments) 
		--											&& (exprListContainsVarNames accesses loopVars) 	-- && (constantCheck_query accesses)
			where
				assignments = arrayAccesses_query expr1
				accesses = arrayAccesses_query expr2
		For _ _ var _ _ _ _ -> foldl (++) "" (gmapQ (mkQ "" (getErrors_map (loopVars ++ [var]) )) codeSeg)
		OpenCLMap _ _ _ _ _ -> foldl (++) "" (gmapQ (mkQ "" (getErrors_map loopVars)) codeSeg)
		_ -> foldl (++) "" (gmapQ (mkQ "" (getErrors_map loopVars)) codeSeg)

--	Appends a new item to the list of annotations already associated to a particular node
addAnnotation :: Fortran [String] -> String -> Fortran [String]
addAnnotation original appendage = case original of
		For anno srcspan var expr1 expr2 expr3 fortran -> For (anno ++ [appendage]) srcspan var expr1 expr2 expr3 fortran
		_ -> original		

--	Used by getErrors_map to format the information on the position of a particular piece of code that is used as the information
--	output to the user
errorLocationFormatting :: SrcSpan -> String
errorLocationFormatting ((SrcLoc filename line column), srcEnd) = "line " ++ show line -- ++ ", column " ++ show column

--	Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new OpenCLReduce nodes or the
--	original sub-tree annotated with reasons why the loop is not a reduction
paralleliseLoop_reduce ::Fortran [String] -> [VarName [String]] -> (Bool, Fortran [String])
paralleliseLoop_reduce loop loopVars = (False, addAnnotation loop (outputTab ++ "Cannot reduce due to:\n" ++ outputTab ++ outputTab ++ "Reduction is not implemented\n"))

--	Top level function that is called on the AST of a program. Function traverses the program unit and applies transformForLoop when a
--	Fortran node is encountered. The function performs a transformation of the input AST and so returns a version whose loops are either
--	annotated or made parallel
paralleliseProgram :: (Typeable p, Data p) => ProgUnit p -> ProgUnit p 
paralleliseProgram = everywhere (mkT (transformForLoop))

--	When a Fortran node is encountered by paralleliseProgram, this function is used. It's functionality is to differentiate between nodes
--	that represent for loops and those that do not. When a for loop is encountered, it is attempted to be parallelised using the 
-- 	paralleliseLoop function
transformForLoop :: Fortran [String] -> Fortran [String]
transformForLoop inp = case inp of
		For _ _ _ _ _ _ _ -> paralleliseLoop [] inp
		_ -> inp

--	Function uses a SYB query to get all of the loop condtions contained within a particular AST. loopCondtions_query traverses the AST
--	and calls getLoopConditions when a Fortran node is encountered.
loopCondtions_query :: (Typeable p, Data p) =>  Fortran p -> [(VarName p, Expr p, Expr p, Expr p)]
loopCondtions_query = everything (++) (mkQ [] getLoopConditions)

getLoopConditions :: (Typeable p, Data p) => Fortran p -> [(VarName p, Expr p, Expr p, Expr p)]
getLoopConditions codeSeg = case codeSeg of
		For _ _ var start end step _ -> [(var, start, end, step)]
		OpenCLMap _ _ _ loopVars _ -> loopVars
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
													-- Bin p SrcSpan  (BinOp p) (Expr p) (Expr p)			

--removeLoopConstructs_trans :: (Typeable p, Data p) => Fortran p -> Fortran p
--removeLoopConstructs_trans = everywhere (mkT (removeLoopConstructs))

--	Takes an AST and removes the loop statements from the node and joins up the rest of the code so that is it represented in the
--	format that language-fortran uses.
removeLoopConstructs_recursive :: Fortran [String] -> Fortran [String]
removeLoopConstructs_recursive (FSeq _ _ (For _ _ _ _ _ _ (FSeq anno srcspan fortran11 fortran12)) fortran02) = FSeq anno srcspan fortran11 
																															(appendFortran_recursive 	(removeLoopConstructs_recursive fortran02) 
																																						(removeLoopConstructs_recursive fortran12))
removeLoopConstructs_recursive (FSeq _ _ (OpenCLMap _ _ _ _ (FSeq anno srcspan fortran11 fortran12)) fortran02) = FSeq anno srcspan fortran11 
																															(appendFortran_recursive 	(removeLoopConstructs_recursive fortran02) 
																																						(removeLoopConstructs_recursive fortran12))
removeLoopConstructs_recursive (For _ _ _ _ _ _ fortran1) = removeLoopConstructs_recursive fortran1
removeLoopConstructs_recursive (OpenCLMap _ _ _ _ fortran1) = removeLoopConstructs_recursive fortran1
removeLoopConstructs_recursive (NullStmt anno srcspan) = NullStmt anno srcspan
removeLoopConstructs_recursive codeSeg = gmapT (mkT removeLoopConstructs_recursive) codeSeg


--removeLoopConstructs :: Fortran [String] -> Fortran [String]
----removeLoopConstructs (FSeq _ _ (For _ _ _ _ _ _ (FSeq _ _ fortran11 fortran12)) fortran02) = FSeq [] generatedSrcSpan fortran11 (appendFortran_trans fortran02 fortran12)
--removeLoopConstructs (FSeq _ _ (For _ _ _ _ _ _ (FSeq _ _ fortran11 fortran12)) fortran02) = FSeq [] generatedSrcSpan fortran11 (appendFortran_recursive fortran02 fortran12)
--removeLoopConstructs (FSeq _ _ (For _ _ _ _ _ _ fortran1) fortran2 ) = FSeq [] generatedSrcSpan fortran1 fortran2
--removeLoopConstructs (FSeq _ _ fortran1 (For _ _ _ _ _ _ fortran2)) = FSeq [] generatedSrcSpan fortran1 fortran2

----removeLoopConstructs (FSeq _ _ (OpenCLMap _ _ _ _ (FSeq _ _ fortran11 fortran12)) fortran02) = FSeq [] generatedSrcSpan fortran11 (appendFortran_trans fortran02 fortran12)
--removeLoopConstructs (FSeq _ _ (OpenCLMap _ _ _ _ (FSeq _ _ fortran11 fortran12)) fortran02) = FSeq [] generatedSrcSpan fortran11 (appendFortran_recursive fortran02 fortran12)
--removeLoopConstructs (FSeq _ _ (OpenCLMap _ _ _ _ fortran1) fortran2 ) = FSeq [] generatedSrcSpan fortran1 fortran2
--removeLoopConstructs (FSeq _ _ fortran1 (OpenCLMap _ _ _ _ fortran2)) = FSeq [] generatedSrcSpan fortran1 fortran2
--removeLoopConstructs (FSeq _ _ fortran1 (NullStmt _ _)) = fortran1

--removeLoopConstructs (For _ _ _ _ _ _ fortran) = fortran
--removeLoopConstructs (OpenCLMap _ _ _ _ fortran) = fortran
--removeLoopConstructs codeSeg = codeSeg

--appendFortran_trans :: Fortran [String] -> Fortran [String] -> Fortran [String]
--appendFortran_trans newFortran codeSeg = everywhere (mkT (appendFortran newFortran)) codeSeg

--appendFortran :: Fortran [String] -> Fortran [String] -> Fortran [String]
--appendFortran newFortran codeSeg = case codeSeg of
--	(FSeq _ _ _ (FSeq _ _ _ _)) -> codeSeg 
-- 	(FSeq _ _ fortran1 fortran2) -> FSeq [] generatedSrcSpan fortran1 (FSeq [] generatedSrcSpan fortran2 newFortran)
-- 	_ -> codeSeg
-- 	-- _ -> FSeq [] generatedSrcSpan codeSeg newFortran

--	Takes two ASTs and appends on onto the other so that the resulting AST is in the correct format
appendFortran_recursive :: Fortran [String] -> Fortran [String] -> Fortran [String]
appendFortran_recursive newFortran (FSeq _ _ _ (FSeq _ _ _ fortran1)) = appendFortran_recursive newFortran fortran1 
appendFortran_recursive newFortran (FSeq _ _ fortran1 fortran2) = FSeq [] generatedSrcSpan fortran1 (FSeq [] generatedSrcSpan fortran2 newFortran)
appendFortran_recursive newFortran codeSeg = FSeq [] generatedSrcSpan codeSeg newFortran

--	Traverses the AST and prooduces a single string that contains all of the parallelising errors for this particular run of the compiler.
--	compileErrorListing traverses the AST and applies getErrors to Fortran nodes (as currently they are the only nodes that have
--	ever have annotations applied. The resulting string is then output to the user.
compileErrorListing :: ProgUnit [String] -> String
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

--	Generic function that takes two lists a and b and returns a list c that is all of the elements of a that do not appear in b.
listSubtract :: Eq a => [a] -> [a] -> [a]
listSubtract a b = filter (\x -> notElem x b) a

--	Generic function that removes all duplicate elements from a list.
listRemoveDuplications :: Eq a => [a] -> [a]
listRemoveDuplications a = foldl (\accum item -> if notElem item accum then accum ++ [item] else accum) [] a

--	Function takes an AST reprenting an expression and returns a nested list representing the elements that are accessed by each term
-- 	of the original expression
arrayAccesses_query :: (Typeable p, Data p) =>  Expr p -> [[Expr p]]
arrayAccesses_query codeSeg = case codeSeg of
	Var _ _ lst -> [foldl concatExprList_foldl [] lst]
	Bin _ _ _ expr1 expr2 -> arrayAccesses_query expr1 ++ arrayAccesses_query expr2
	_ -> []

-- (gmapQ (mkQ "" (getErrors_map loopVars)) codeSeg)

--arrayAccesses_query :: (Typeable p, Data p) =>  Expr p -> [Expr p]
--arrayAccesses_query = everything (++) (mkQ [] getArrayAccesses)

--getArrayAccesses :: (Typeable p, Data p) => Expr p -> [Expr p]
--getArrayAccesses codeSeg = case codeSeg of
--	Var _ _ lst -> foldl concatExprList_foldl [] lst
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

generatedSrcSpan :: SrcSpan
generatedSrcSpan = (SrcLoc {srcFilename = "GENERATED", srcLine = -1, srcColumn = -1}, SrcLoc {srcFilename = "GENERATED", srcLine = -1, srcColumn = -1})

test_exprListContainsAllVarNames :: (Typeable p, Data p, Eq p) =>  [Expr p] -> [VarName p] -> [VarName p]
test_exprListContainsAllVarNames container contains = (foldl delete_foldl (listRemoveDuplications contains) (everything (++) (mkQ [] getVarNames) container))