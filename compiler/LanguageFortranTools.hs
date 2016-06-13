module LanguageFortranTools where

--	This module contains a set of functions that are used all over the source for the compiler. Essentially a utility module.

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Data.Typeable
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List
import Data.Maybe
import System.Process
import System.Directory
import Text.Read
import qualified Data.Map as DMap

import PreProcessor

type Anno = DMap.Map (String) [String]

--	Type used when determining allowed values for iterator variables. Holds the currently chosen values
--	for previous iterator variables that allow the calculation of inner iterator variables in the case
--	of nested loops whose bounds depends on previous iterator variables.
--	Also used during constant folding to hold current constants
type ValueTable = DMap.Map String (Float, BaseType Anno)

nullAnno :: Anno
nullAnno = DMap.empty

--	Taken from language-fortran example. Runs preprocessor on target source and then parses the result, returning an AST.
parseFile cppArgs filename = do 
								let dFlagList = foldl (\accum item -> accum ++ ["-D", item]) [] cppArgs
								inp <- (readProcess "cpp" ([filename] ++ dFlagList ++ ["-P"]) "") 
								return $ parse $ preProcess inp

cpp cppArgs filename = do 	
							let dFlagList = foldl (\accum item -> accum ++ ["-D", item]) [] cppArgs
							inp <- (readProcess "cpp" ([filename] ++ dFlagList ++ ["-P"]) "") 
							return inp

--	Used by analyseLoop_map to format the information on the position of a particular piece of code that is used as the information
--	output to the user
errorLocationFormatting :: SrcSpan -> String
errorLocationFormatting ((SrcLoc filename line column), srcEnd) = show line ++ ":" ++ show column --"line " ++ show line ++ ", column " ++ show column

errorLocationRangeFormatting :: SrcSpan -> String
errorLocationRangeFormatting ((SrcLoc _ line_start _), (SrcLoc _ line_end _)) = "line " ++ show line_start ++ " and line " ++ show line_end -- ++ ", column " ++ show column

outputExprFormatting :: Expr Anno -> String
outputExprFormatting (Var _ _ list) = foldl (++) "" (map (\(varname, exprList) -> ((\(VarName _ str) -> str) varname) ++ 
															(if exprList /= [] then "(" ++ (foldl (\accum item -> (if accum /= "" then accum ++ "," else "") 
																++ item) "" (map (outputExprFormatting) exprList)) ++ ")" else "")) list)
outputExprFormatting (Con _ _ str) = str
outputExprFormatting (Bin _ _ op expr1 expr2) = "(" ++ outputExprFormatting expr1 ++ " " ++ op_str ++ " " ++ outputExprFormatting expr2 ++ ")"
							where
								op_str = case op of
									Plus p -> "+"
									Minus p -> "-"
									Mul p -> "*"
									Div p -> "/"
									Or p -> ".OR."
									And p -> ".AND."
									Concat p -> "//"
									Power p -> "**"
									RelEQ p -> "=="
									RelNE p -> "/="
									RelLT p -> "<"
									RelLE p -> "<="
									RelGT p -> ">"
									RelGE p -> ">="
outputExprFormatting (NullExpr _ _) = ""
outputExprFormatting (Null _ _) = "null"
outputExprFormatting (Unary _ _ unOp expr) = "(" ++ op_str ++ outputExprFormatting expr ++ ")"
							where 
								op_str = case unOp of
									UMinus p -> "-"
									Not p -> ".NOT."

outputExprFormatting codeSeg = show codeSeg

orElem :: Eq a => a -> [[a]] -> Bool
orElem item [] = False
orElem item (firstList:lists) = elem item firstList || orElem item lists

listExtractSingleAppearances :: Eq a => [a] -> [a] 
listExtractSingleAppearances list = listExtractSingleAppearances' list list

listExtractSingleAppearances' :: Eq a => [a] -> [a] -> [a]
listExtractSingleAppearances' (x:[]) wholeList = if appearences == 1 then [x] else []
		where
			appearences = listCountAppearences x wholeList
listExtractSingleAppearances' (x:tailList) wholeList = if appearences == 1 then [x] ++ listExtractSingleAppearances' tailList wholeList else listExtractSingleAppearances' tailList wholeList
		where
			appearences = listCountAppearences x wholeList

listCountAppearences :: Eq a => a -> [a] -> Int
listCountAppearences value (x:[]) 	| 	value == x = 1
									| 	otherwise = 0
listCountAppearences value (x:list) 	| 	value == x = 1 + listCountAppearences value list
									| 	otherwise = 0 + listCountAppearences value list

--	Generic function that removes all duplicate elements from a list.
listRemoveDuplications :: Eq a => [a] -> [a]
listRemoveDuplications a = foldl (\accum item -> if notElem item accum then accum ++ [item] else accum) [] a

listConcatUnique :: Eq a => [a] -> [a] -> [a]
listConcatUnique a b = foldl (\accum item -> if notElem item accum then accum ++ [item] else accum) b a

--	Used by SYB query to extract expressions
extractExpr :: Expr Anno -> Expr Anno 
extractExpr expr = expr

extractExpr_list :: Expr Anno -> [Expr Anno] 
extractExpr_list expr = [expr]

--	Used to break down a tree of expressions that might form a calculation into a list of expressions for analysis.
extractOperands :: (Typeable p, Data p) => Expr p -> [Expr p]
extractOperands (Bin _ _ _ expr1 expr2) = extractOperands expr1 ++ extractOperands expr2
extractOperands expr 					= [expr]

--	Used to extract the name of a Var in a particular expression
extractVarNames :: (Typeable p, Data p) => Expr p -> [VarName p]
extractVarNames (Var _ _ lst) = map (\(x, _) -> x) lst
extractVarNames _ = []

--	Used to extract array index expressions and function call arguments.
extractContainedVars :: (Typeable p, Data p) => Expr p -> [Expr p]
extractContainedVars (Var _ _ lst) = foldl (\accumExprs (itemVar, itemExprs) -> accumExprs ++ itemExprs) [] lst
extractContainedVars _ = []

extractContainedOperands :: (Typeable p, Data p) => Expr p -> [Expr p]
extractContainedOperands expr =  foldl (\accum item -> accum ++ (extractOperands item)) [] containedVars
				where
					containedVars = extractContainedVars expr

extractAssignments :: Fortran Anno -> [Fortran Anno]
extractAssignments codeSeg = case codeSeg of 
								Assg _ _ _ _ -> [codeSeg]
								_	-> []

extractFortran :: Fortran Anno -> [Fortran Anno]
extractFortran fort = [fort]

extractDecl :: Decl Anno -> [Decl Anno]
extractDecl decl = [decl]

extractAssigneeFromDecl :: Decl Anno -> VarName Anno
extractAssigneeFromDecl (Decl anno src lst typ) = head (extractVarNames assignee)
			where
				assignee = (\(x, _, _) -> x) (head lst)
extractAssigneeFromDecl	_ = error "extractAssigneeFromDecl"

-- extractBaseType :: Decl Anno -> BaseType Anno
-- extractBaseType (Decl _ _ _ (BaseType _ bt _ _ _)) = bt
-- extractBaseType (Decl _ _ _ (ArrayT _ _ bt _ _ _)) = bt

extractBaseType :: Type Anno -> BaseType Anno
extractBaseType (BaseType _ bt _ _ _) = bt
extractBaseType (ArrayT _ _ bt _ _ _) = bt

extractBlock :: Block Anno -> [Block Anno]
extractBlock block = [block]

extractVarNames_loopVars :: [(VarName Anno, Expr Anno, Expr Anno, Expr Anno)] -> [VarName Anno]
extractVarNames_loopVars = map (\(x,_,_,_) -> x) 

--	Generates a SrcSpan that is attached to nodes that have been generated by this program
nullSrcSpan :: SrcSpan
nullSrcSpan = (SrcLoc {srcFilename = "generated", srcLine = -1, srcColumn = -1}, SrcLoc {srcFilename = "generated", srcLine = -1, srcColumn = -1})

-- generateSrcSpan :: SrcSpan -> SrcSpan
-- generateSrcSpan ((SrcLoc sFile sLine sCol), (SrcLoc eFile eLine eCol)) = (SrcLoc {srcFilename = "generated", srcLine = sLine, srcColumn = sCol}, SrcLoc {srcFilename = "generated", srcLine = eLine, srcColumn = eCol})

generateSrcSpan :: String -> SrcSpan -> SrcSpan
generateSrcSpan [] ((SrcLoc sFile sLine sCol), (SrcLoc eFile eLine eCol)) = (SrcLoc {srcFilename = "generated", srcLine = sLine, srcColumn = sCol}, SrcLoc {srcFilename = "generated", srcLine = eLine, srcColumn = eCol})
generateSrcSpan filename ((SrcLoc sFile sLine sCol), (SrcLoc eFile eLine eCol)) = (SrcLoc {srcFilename = filename, srcLine = sLine, srcColumn = sCol}, SrcLoc {srcFilename = filename, srcLine = eLine, srcColumn = eCol})

generateAssgCode :: Expr Anno -> Expr Anno -> Fortran Anno
generateAssgCode expr1 expr2 = Assg nullAnno nullSrcSpan expr1 expr2 

generateLTExpr :: Expr Anno -> Expr Anno -> Expr Anno
generateLTExpr expr1 expr2 = Bin nullAnno nullSrcSpan (RelLT nullAnno) expr1 expr2

generateAndExpr :: Expr Anno -> Expr Anno -> Expr Anno
generateAndExpr expr1 expr2 = Bin nullAnno nullSrcSpan (And nullAnno) expr1 expr2

generateAndExprFromList :: [Expr Anno] -> Expr Anno
generateAndExprFromList list = foldl1 (generateAndExpr) list

generateVar :: VarName Anno -> Expr Anno
generateVar varname = Var nullAnno nullSrcSpan [(varname, [])]

generateArrayVar :: VarName Anno -> [Expr Anno] -> Expr Anno
generateArrayVar varname exprs = Var nullAnno nullSrcSpan [(varname, exprs)]

generateIntConstant :: Int -> Expr Anno
generateIntConstant value = Con nullAnno nullSrcSpan (show value)

generateFloatConstant :: Float -> Expr Anno
generateFloatConstant value = Con nullAnno nullSrcSpan (show value)

-- generateArrayVar :: VarName Anno -> Expr Anno -> Expr Anno
-- generateArrayVar varname access = Var nullAnno nullSrcSpan [(varname, [access])]

generateIf :: Expr Anno -> Fortran Anno -> Fortran Anno
generateIf expr fortran = If nullAnno nullSrcSpan expr fortran [] Nothing

generateAdditionExpr :: Expr Anno -> Expr Anno -> Expr Anno
generateAdditionExpr expr1 expr2 = Bin nullAnno nullSrcSpan (Plus nullAnno) expr1 expr2

generateProductExpr :: Expr Anno -> Expr Anno -> Expr Anno
generateProductExpr expr1 expr2 = Bin nullAnno nullSrcSpan (Mul nullAnno) expr1 expr2

generateSubtractionExpr :: Expr Anno -> Expr Anno -> Expr Anno
generateSubtractionExpr expr1 expr2 = Bin nullAnno nullSrcSpan (Minus nullAnno) expr1 expr2

generateDivisionExpr :: Expr Anno -> Expr Anno -> Expr Anno
generateDivisionExpr expr1 expr2 = Bin nullAnno nullSrcSpan (Div nullAnno) expr1 expr2

getUses :: Uses Anno -> [Uses Anno]
getUses uses = case uses of 
					(Use _ _ _ _) -> [uses]
					_ -> []

getUnitName :: ProgUnit Anno -> String
getUnitName progunit = foldl (++) [] (gmapQ (mkQ [] getUnitName') progunit)

getUnitName' :: SubName Anno -> String
getUnitName' (SubName _ str) = str
getUnitName' _ = ""

-- 	Function returns the loop variable for an AST representing a for loop
getLoopVar :: Fortran p -> Maybe(VarName p)
getLoopVar (For _ _ var _ _ _ _) = Just var
getLoopVar _ = Nothing

--	Used to standardise SrcSpans so that nodes of an AST may be matched up even if they appear in completely different
--	parts of a program. Also used to signify that a node has been changed and cannot be copied from the orignal source during code
--	generation
applyGeneratedSrcSpans :: (Data (a Anno)) => a Anno -> a Anno
applyGeneratedSrcSpans = everywhere (mkT (standardiseSrcSpan))

standardiseSrcSpan :: SrcSpan -> SrcSpan
standardiseSrcSpan src = nullSrcSpan

hasOperand :: Expr Anno -> Expr Anno -> Bool
hasOperand container contains = all (== True) $ map (\x -> elem x (extractOperands $ applyGeneratedSrcSpans container)) (extractOperands $ applyGeneratedSrcSpans contains)

--	Appends a new item to the list of annotations already associated to a particular node
appendAnnotation :: Fortran Anno -> String -> String -> Fortran Anno
appendAnnotation original key appendage = case original of
		For anno f2 f3 f4 f5 f6 f7 -> For (appendToMap key appendage anno) f2 f3 f4 f5 f6 f7
		OpenCLMap anno f2 f3 f4 f5 f6 -> OpenCLMap (appendToMap key appendage anno) f2 f3 f4 f5 f6
		OpenCLReduce anno f2 f3 f4 f5 f6 f7 -> OpenCLReduce (appendToMap key appendage anno) f2 f3 f4 f5 f6 f7
		_ -> original

		-- appendToMap

appendAnnotationList :: Fortran Anno -> String -> [String] -> Fortran Anno
appendAnnotationList original key appendage = foldl (\accum item -> appendAnnotation accum key item) original appendage

appendAnnotationMap :: Fortran Anno -> Anno -> Fortran Anno
appendAnnotationMap codeSeg newMap = case codeSeg of
		For anno f2 f3 f4 f5 f6 f7 -> For (combineAnnotations newMap anno) f2 f3 f4 f5 f6 f7
		OpenCLMap anno f2 f3 f4 f5 f6 -> OpenCLMap (combineAnnotations newMap anno) f2 f3 f4 f5 f6
		OpenCLReduce anno f2 f3 f4 f5 f6 f7 -> OpenCLReduce (combineAnnotations newMap anno) f2 f3 f4 f5 f6 f7

-- appendAnnotation original appendage = original

--	Prepends a new item to the list of annotations already associated to a particular node
-- prependAnnotation :: Fortran Anno -> String -> Fortran Anno
-- prependAnnotation original appendage = case original of
-- 		_ -> original

removeAllAnnotations original = everywhere (mkT removeAnnotations) original

removeAnnotations :: Fortran Anno -> Fortran Anno
removeAnnotations original = case original of
		For anno f2 f3 f4 f5 f6 f7 -> For nullAnno f2 f3 f4 f5 f6 f7
		OpenCLMap anno f2 f3 f4 f5 f6 -> OpenCLMap nullAnno f2 f3 f4 f5 f6
		OpenCLReduce anno f2 f3 f4 f5 f6 f7 -> OpenCLReduce nullAnno f2 f3 f4 f5 f6 f7
		_ -> original

combineAnnotations :: Anno -> Anno -> Anno
combineAnnotations a b = combineMaps a b


usesVarName_list :: [VarName Anno] -> Expr Anno -> Bool
usesVarName_list loopWrites (Var _ _ list) = foldl (||) False $ map (\(varname, exprs) -> elem varname loopWrites) list
usesVarName_list loopWrites (Bin _ _ _ expr1 expr2) = (usesVarName_list loopWrites expr1) || (usesVarName_list loopWrites expr2)
usesVarName_list loopWrites _ = False

usesVarName :: VarName Anno -> Expr Anno -> Bool
usesVarName varnameInp (Var _ _ list) = foldl (||) False $ map (\(varname, exprs) -> varname == varnameInp) list

isVar :: Expr Anno -> Bool
isVar (Var _ _ _) = True
isVar _ = False

extractLoopVars :: Fortran Anno -> [VarName Anno]
extractLoopVars codeSeg = everything (++) (mkQ [] extractLoopVars') codeSeg

extractLoopVars' :: Fortran Anno -> [VarName Anno]
extractLoopVars' (For _ _ var _ _ _ _) = [var]
extractLoopVars' _ = []

extractUsedVarName :: Expr Anno -> [VarName Anno]
extractUsedVarName (Var _ _ list) = map (\(varname, exprs) -> varname) list
extractUsedVarName _ = []

replaceAllOccurences_varnamePairs :: Fortran Anno -> [VarName Anno] -> [VarName Anno] -> Fortran Anno
replaceAllOccurences_varnamePairs codeSeg originals replacements = foldl (\accum (v1, v2) -> replaceAllOccurences_varname accum v1 v2) codeSeg pairs
					where
						pairs = zip originals replacements

replaceAllOccurences_varname :: (Data (a Anno)) => a Anno -> VarName Anno -> VarName Anno -> a Anno
replaceAllOccurences_varname codeSeg original replacement = everywhere (mkT (replaceVarname original replacement)) codeSeg

replaceVarname :: VarName Anno -> VarName Anno -> VarName Anno -> VarName Anno
replaceVarname original replacement inp 	| 	original == inp = replacement
											|	otherwise = inp


varNameStr :: VarName Anno -> String
varNameStr (VarName _ str) = str

--	Takes two ASTs and appends on onto the other so that the resulting AST is in the correct format
appendFortran_recursive :: Fortran Anno -> Fortran Anno -> Fortran Anno
appendFortran_recursive newFortran (FSeq anno1 src1 fortran1 (NullStmt anno2 src2)) = FSeq anno1 src1 fortran1 newFortran
appendFortran_recursive newFortran (FSeq anno1 src1 fortran1 fortran2) = FSeq anno1 src1 fortran1 (appendFortran_recursive newFortran fortran2)
appendFortran_recursive newFortran codeSeg = FSeq nullAnno nullSrcSpan codeSeg newFortran

--	Takes an AST and removes the loop statements from the node and joins up the rest of the code so that is it represented in the
--	format that language-fortran uses.
removeLoopConstructs_recursive :: Fortran Anno -> Fortran Anno
removeLoopConstructs_recursive (FSeq anno _ (For _ _ _ _ _ _ fortran1) fortran2) = removeLoopConstructs_recursive $ appendFortran_recursive fortran2 fortran1
removeLoopConstructs_recursive (For _ _ _ _ _ _ fortran) = removeLoopConstructs_recursive fortran
removeLoopConstructs_recursive (FSeq _ _ fortran (NullStmt _ _)) = removeLoopConstructs_recursive fortran
removeLoopConstructs_recursive codeSeg = codeSeg

extractFirstChildFor :: Fortran Anno -> Maybe(Fortran Anno)
extractFirstChildFor (For _ _ _ _ _ _ fortran) = firstFor
		where
			allFors = everything (++) (mkQ [] extractFor) fortran
			firstFor = case allFors of
						[] -> Nothing
						otherwise -> Just(head allFors)

extractFor :: Fortran Anno -> [Fortran Anno]
extractFor codeSeg = case codeSeg of
						For _ _ _ _ _ _ _ -> [codeSeg]
						_ -> []

extractLineNumber :: SrcSpan -> Int
extractLineNumber ((SrcLoc _ line _), _) = line

getEarliestSrcSpan :: [SrcSpan] -> Maybe(SrcSpan)
getEarliestSrcSpan [] = Nothing
getEarliestSrcSpan spans = Just (foldl (\accum item -> if checkSrcSpanBefore item accum then item else accum) (spans!!0) spans)

checkSrcSpanBefore :: SrcSpan -> SrcSpan -> Bool
checkSrcSpanBefore ((SrcLoc file_before line_before column_before), beforeEnd) ((SrcLoc file_after line_after column_after), afterEnd) = (line_before < line_after) || ((line_before == line_after) && (column_before < column_after))

checkSrcSpanBefore_line :: SrcSpan -> SrcSpan -> Bool
checkSrcSpanBefore_line ((SrcLoc file_before line_before column_before), beforeEnd) ((SrcLoc file_after line_after column_after), afterEnd) = (line_before < line_after)

generateSrcSpanMerge :: SrcSpan -> SrcSpan -> SrcSpan
generateSrcSpanMerge src1 src2 = (src1_s, src2_e)
					where
						(src1_s, src1_e) = src1
						(src2_s, src2_e) = src2

getSrcSpanNonIntersection :: SrcSpan -> SrcSpan -> (SrcSpan, SrcSpan)
getSrcSpanNonIntersection src1 src2 = (firstSrc, secondSrc)
					where
						(src1_s, src1_e) = src1
						(src2_s, src2_e) = src2

						firstSrc = (src1_s, src2_s)
						secondSrc = (src2_e, src1_e)

listCartesianProduct :: [a] -> [a] -> [(a,a)]
listCartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

--	Generic function that takes two lists a and b and returns a +list c that is all of the elements of a that do not appear in b.
listSubtract :: Eq a => [a] -> [a] -> [a]
listSubtract a b = filter (\x -> notElem x b) a

listIntersection :: Eq a => [a] -> [a] -> [a]
listIntersection a b = filter (\x -> elem x b) a

combineMaps :: Ord k => DMap.Map k [a] -> DMap.Map k [a] -> DMap.Map k [a]
combineMaps map1 map2 = resultantAnalysis
						where
							map2List = DMap.toList map2
							resultantAnalysis = foldl (\accum (key, value) -> DMap.insert key ((DMap.findWithDefault [] key accum) ++ value) accum) map1 map2List

appendToMap :: Ord k => k -> a -> DMap.Map k [a] -> DMap.Map k [a]
appendToMap key item map = DMap.insert key ((DMap.findWithDefault [] key map) ++ [item]) map

extractPrimaryReductionOp :: Expr Anno -> Expr Anno -> Maybe(BinOp Anno)
extractPrimaryReductionOp assignee (Bin _ _ op expr1 expr2) = case assigneePresent of
										True -> Just op
										False -> childOp
						where
							primaryOp1 = extractPrimaryReductionOp assignee expr1
							primaryOp2 = extractPrimaryReductionOp assignee expr2 
							childOp = case primaryOp1 of
										Just a -> primaryOp1
										Nothing ->  primaryOp2
							assigneePresent = applyGeneratedSrcSpans expr1 == applyGeneratedSrcSpans assignee ||
												applyGeneratedSrcSpans expr2 == applyGeneratedSrcSpans assignee
extractPrimaryReductionOp assignee assignment = Nothing

extractPrimaryReductionFunction ::  Expr Anno -> Expr Anno -> String
extractPrimaryReductionFunction assignee (Var _ _ list) = foldl assigneePresent "" standardisedList
						where
							assigneePresent = (\accum (var, exprList) -> if elem (applyGeneratedSrcSpans assignee) exprList then varNameStr var else accum)
							standardisedList = map (\(var, exprList) -> (var, map (applyGeneratedSrcSpans) exprList)) list
extractPrimaryReductionFunction assignee expr = "" -- error ("Error: extractPrimaryReductionFunction\nType: " ++ (show $ typeOf expr) ++ "\nShow: " ++ (show expr))

-- evaluateRange_int :: ValueTable -> Expr Anno -> Expr Anno -> Expr Anno -> [Int]
-- evaluateRange_int vt startExpr endExpr stepExpr = map (round) (evaluateRange vt startExpr endExpr stepExpr)

evaluateRange :: ValueTable -> Expr Anno -> Expr Anno -> Expr Anno -> Maybe([Float])
evaluateRange vt startExpr endExpr stepExpr = range
		where
			startInt = evaluateExpr vt startExpr
			endInt = evaluateExpr vt endExpr
			stepInt = evaluateExpr vt stepExpr

			range = case startInt of
						Nothing -> Nothing
						Just start -> case endInt of
										Nothing -> Nothing
										Just end -> case stepInt of
														Nothing -> Nothing
														Just step -> Just (map (fromIntegral) [round start :: Int,(round start :: Int)+(round step :: Int) ..round end :: Int])
-- evaluateExpr_int :: ValueTable -> Expr Anno -> Maybe(Int)
-- evaluateExpr_int vt expr = case evaluateExpr vt expr of
-- 								Nothing -> Nothing
-- 								Just result -> Just (round result)

evaluateExpr :: ValueTable -> Expr Anno -> Maybe(Float)
evaluateExpr vt expr = extractEvaluatedValue (evaluateExpr_type vt expr)

extractEvaluatedValue :: Maybe(Float, BaseType Anno) -> Maybe(Float)
extractEvaluatedValue expr = case expr of
							Nothing -> Nothing
							Just (val, _) -> Just val

extractEvaluatedType :: Maybe(Float, BaseType Anno) -> Maybe(BaseType Anno)
extractEvaluatedType expr = case expr of
							Nothing -> Nothing
							Just (_, typ) -> Just typ

evaluateExpr_type :: ValueTable -> Expr Anno -> Maybe(Float, BaseType Anno)
evaluateExpr_type vt (Bin _ _ binOp expr1 expr2) = case binOp of
												Plus _ -> 	maybeBinOp 			expr1_eval expr2_eval (+)
												Minus _ -> 	maybeBinOp 			expr1_eval expr2_eval (-)
												Mul _ -> 	maybeBinOp 			expr1_eval expr2_eval (*)
												Div _ -> 	case extractEvaluatedType expr1_eval of
																Just (Real _) -> 	maybeBinOp expr1_eval expr2_eval (/)
																Just (Integer _) -> case extractEvaluatedType expr2_eval of
																						Just (Real _) -> 	maybeBinOp expr1_eval expr2_eval (/)
																						Just (Integer _) -> maybeBinOp_integral expr1_eval expr2_eval (quot)
																						Nothing -> Nothing
																Nothing -> Nothing
												Power _ -> 	maybeBinOp_integral expr1_eval expr2_eval (^)
												_ -> Nothing
			where
				expr1_eval = evaluateExpr_type vt expr1
				expr2_eval = evaluateExpr_type vt expr2
evaluateExpr_type vt (Unary _ _ unOp expr) = case unOp of 
												UMinus _ -> maybeNegative (evaluateExpr_type vt expr)
												Not _ -> Nothing
evaluateExpr_type vt (Var p src lst)   	| varString == "mod" = maybeBinOp_integral (evaluateExpr_type vt expr1) (evaluateExpr_type vt expr2) (mod)
										| otherwise = lookupValueTable_type varString vt--DMap.lookup varString vt
			where
				varString = varNameStr $ head $ extractUsedVarName (Var p src lst)
				headExprList = snd (head lst)
				expr1 = head headExprList
				expr2 = head $ tail headExprList
evaluateExpr_type _ (Con _ _ str)	|	head str == '.' = Just ((read $ tail str) :: Float, Real nullAnno)
									|	last str == '.' = Just ((read $ take (length str - 1) str) :: Float, Real nullAnno)
									|	elem '.' str = Just(read str :: Float, Real nullAnno)
									|	otherwise = Just(read str :: Float, Integer nullAnno)
evaluateExpr_type _ _ = Nothing

lookupValueTable :: String -> ValueTable -> Maybe(Float)
lookupValueTable str table = value
			where
				(value, typ) =  case lookupValueTable_type str table of
									Nothing -> (Nothing, Nothing)
									Just (v, t) -> (Just v, Just t)

lookupValueTable_type :: String -> ValueTable -> Maybe(Float, BaseType Anno)
lookupValueTable_type str table =  DMap.lookup str table

addToValueTable :: VarName Anno -> Float -> ValueTable -> ValueTable
addToValueTable var value table = addToValueTable_type var value (Real nullAnno) table -- DMap.insert (varNameStr var) value table

addToValueTable_type :: VarName Anno -> Float -> BaseType Anno -> ValueTable -> ValueTable
addToValueTable_type var value typ table = DMap.insert (varNameStr var) (value, typ) table

deleteValueFromTable :: VarName Anno -> ValueTable -> ValueTable
deleteValueFromTable var table = DMap.delete (varNameStr var) table

-- maybeQuotOp :: Maybe(Float) -> Maybe(Float) -> Float
-- maybeQuotOp maybeFloat1 maybeFloat2 = case maybeFloat1 of
-- 											Nothing -> 0.0
-- 											Just float1 -> case maybeFloat2 of
-- 															Nothing -> 0.0
-- 															Just float2 -> fromIntegral (quot (round float1) (round float2)) :: Float

maybeBinOp_integral :: Integral a => Maybe(Float, BaseType Anno) -> Maybe(Float, BaseType Anno) -> (a -> a -> a) -> Maybe(Float, BaseType Anno)
maybeBinOp_integral maybeFloat1 maybeFloat2 op = resultValue
			where
				resultValue = case maybeFloat1 of
											Nothing -> Nothing
											Just (float1, typ1) -> case maybeFloat2 of
															Nothing -> Nothing
															Just (float2, typ2) -> Just(fromIntegral (op (round float1) (round float2)) :: Float, Integer nullAnno)

maybeBinOp :: Maybe(Float, BaseType Anno) -> Maybe(Float, BaseType Anno) -> (Float -> Float -> Float) -> Maybe(Float, BaseType Anno)
maybeBinOp maybeFloat1 maybeFloat2 op = case maybeFloat1 of
											Nothing -> Nothing
											Just (float1, typ1) -> case maybeFloat2 of
															Nothing -> Nothing
															Just (float2, typ2) -> Just(op float1 float2, resolveType typ1 typ2)

--	ASSUME USE OF ONLY TWO TYPES (INTEGER AND REAL) IN EXPRESSION EVALUATION
resolveType :: BaseType Anno -> BaseType Anno -> BaseType Anno
resolveType type1 type2 | 	type1 == type2 = type1
						|	otherwise = Real nullAnno

maybeNegative :: Maybe(Float, BaseType Anno) -> Maybe(Float, BaseType Anno)
maybeNegative (Just(int, typ)) = Just(-int, typ)
maybeNegative Nothing = Nothing

trimFront :: String -> String
trimFront inp = filter (\x -> x /= ' ' && x /= '\t') inp

--	Value used as a global spacing measure. Used for output formatting.
outputTab :: String
outputTab = "  "

compilerName :: String
compilerName = "ParallelFortran"

-- commentSeparator :: String
-- commentSeparator = "!" ++ (take 40 ['-','-'..'-']) ++ "\n"

commentSeparator :: String -> String
commentSeparator str = prefix ++ (commentSeparator' comment body) ++ suffix
	where
		prefix = "! ----"
		suffix = "\n"
		body = (take 122 ['-','-'..'-'])
		comment = case str of 
					[] -> ""
					_ -> (" " ++ str ++ " ")

commentSeparator' :: String -> String -> String
commentSeparator' [] (_:seps) = seps
commentSeparator' comment [] = comment
commentSeparator' (x:xs) (sep:seps) = [x] ++ (commentSeparator' xs seps)

extractIndent :: String -> String
extractIndent (' ':str) = " " ++ extractIndent str
extractIndent _ = ""