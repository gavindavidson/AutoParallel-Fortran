module LanguageFortranTools where

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List
import System.Process
import System.Directory

import PreProcessor

--	Taken from language-fortran example. Runs preprocessor on target source and then parses the result, returning an AST.
parseFile s = do inp <- readProcess "cpp" [s, "-D", "NO_IO", "-P"] "" 
                 return $ parse $ preProcess inp

cpp s = do 	inp <- readProcess "cpp" [s, "-D", "NO_IO", "-P"] "" 
        	return inp

--	Used by analyseLoop_map to format the information on the position of a particular piece of code that is used as the information
--	output to the user
errorLocationFormatting :: SrcSpan -> String
errorLocationFormatting ((SrcLoc filename line column), srcEnd) = show line ++ ":" ++ show column --"line " ++ show line ++ ", column " ++ show column

errorLocationRangeFormatting :: SrcSpan -> String
errorLocationRangeFormatting ((SrcLoc _ line_start _), (SrcLoc _ line_end _)) = "line " ++ show line_start ++ " and line " ++ show line_end -- ++ ", column " ++ show column

errorExprFormatting :: Expr [String] -> String
errorExprFormatting (Var _ _ list) = foldl (++) "" (map (\(varname, exprList) -> ((\(VarName _ str) -> str) varname) ++ 
															(if exprList /= [] then "(" ++ (foldl (\accum item -> (if accum /= "" then accum ++ "," else "") 
																++ item) "" (map (errorExprFormatting) exprList)) ++ ")" else "")) list)
errorExprFormatting (Con _ _ str) = str
errorExprFormatting (Bin _ _ op expr1 expr2) = errorExprFormatting expr1 ++ " " ++ op_str ++ " " ++ errorExprFormatting expr2
							where
								op_str = case op of
									Plus p -> "+"
									Minus p -> "-"
									Mul p -> "*"
									Div p -> "/"
									Or p -> " or "
									And p -> " and "
									Concat p -> " concat "
									Power p -> "^^"
									RelEQ p -> "=="
									RelNE p -> "/="
									RelLT p -> "<"
									RelLE p -> "<="
									RelGT p -> ">"
									RelGE p -> ">="
errorExprFormatting (Null _ _) = "null"					
errorExprFormatting codeSeg = show codeSeg

--	Generic function that removes all duplicate elements from a list.
listRemoveDuplications :: Eq a => [a] -> [a]
listRemoveDuplications a = foldl (\accum item -> if notElem item accum then accum ++ [item] else accum) [] a

--	Used by SYB query to extract expressions
extractExprs :: Expr [String] -> Expr [String] 
extractExprs expr = expr

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

--	Generates a SrcSpan that is attached to nodes that have been generated by this program
nullSrcSpan :: SrcSpan
nullSrcSpan = (SrcLoc {srcFilename = "generated", srcLine = -1, srcColumn = -1}, SrcLoc {srcFilename = "generated", srcLine = -1, srcColumn = -1})

generateSrcSpan :: SrcSpan -> SrcSpan
generateSrcSpan ((SrcLoc sFile sLine sCol), (SrcLoc eFile eLine eCol)) = (SrcLoc {srcFilename = "generated", srcLine = sLine, srcColumn = sCol}, SrcLoc {srcFilename = "generated", srcLine = eLine, srcColumn = eCol})

generateLTExpr :: Expr [String] -> Expr [String] -> Expr [String]
generateLTExpr expr1 expr2 = Bin [] nullSrcSpan (RelLT []) expr1 expr2

generateAndExpr :: Expr [String] -> Expr [String] -> Expr [String]
generateAndExpr expr1 expr2 = Bin [] nullSrcSpan (And []) expr1 expr2

generateAndExprFromList :: [Expr [String]] -> Expr [String]
generateAndExprFromList list = foldl1 (generateAndExpr) list

generateVar :: VarName [String] -> Expr [String]
generateVar varname = Var [] nullSrcSpan [(varname, [])]

generateIf :: Expr [String] -> Fortran [String] -> Fortran [String]
generateIf expr fortran = If [] nullSrcSpan expr fortran [] Nothing

--	Used to standardise SrcSpans so that nodes of an AST may be matched up even if they appear in completely different
--	parts of a program
--standardiseSrcSpan_trans ::(Data a, Typeable a) =>  Expr a -> Expr a
standardiseSrcSpan_trans :: Expr [String] -> Expr [String]
standardiseSrcSpan_trans = everywhere (mkT (standardiseSrcSpan))

standardiseSrcSpan :: SrcSpan -> SrcSpan
standardiseSrcSpan src = nullSrcSpan

hasOperand :: Expr [String] -> Expr [String] -> Bool
hasOperand container contains = all (== True) $ map (\x -> elem x (extractOperands $ standardiseSrcSpan_trans container)) (extractOperands $ standardiseSrcSpan_trans contains)

--	Appends a new item to the list of annotations already associated to a particular node
appendAnnotation :: Fortran [String] -> String -> Fortran [String]
appendAnnotation original appendage = case original of
		For anno f2 f3 f4 f5 f6 f7 -> For (anno ++ [appendage]) f2 f3 f4 f5 f6 f7
		OpenCLMap anno f2 f3 f4 f5 f6 -> OpenCLMap (anno ++ [appendage]) f2 f3 f4 f5 f6
		OpenCLReduce anno f2 f3 f4 f5 f6 f7 -> OpenCLReduce (anno ++ [appendage]) f2 f3 f4 f5 f6 f7
		_ -> original

--	Prepends a new item to the list of annotations already associated to a particular node
prependAnnotation :: Fortran [String] -> String -> Fortran [String]
prependAnnotation original appendage = case original of
		For anno f2 f3 f4 f5 f6 f7 -> For ([appendage] ++ anno) f2 f3 f4 f5 f6 f7
		OpenCLMap anno f2 f3 f4 f5 f6 -> OpenCLMap ([appendage] ++ anno) f2 f3 f4 f5 f6
		OpenCLReduce anno f2 f3 f4 f5 f6 f7 -> OpenCLReduce ([appendage] ++ anno) f2 f3 f4 f5 f6 f7
		_ -> original

--removeAllAnnotations :: Fortran [String] -> Fortran [String]
removeAllAnnotations original = everywhere (mkT removeAnnotations) original

removeAnnotations :: Fortran [String] -> Fortran [String]
removeAnnotations original = case original of
		For anno f2 f3 f4 f5 f6 f7 -> For [] f2 f3 f4 f5 f6 f7
		OpenCLMap anno f2 f3 f4 f5 f6 -> OpenCLMap [] f2 f3 f4 f5 f6
		OpenCLReduce anno f2 f3 f4 f5 f6 f7 -> OpenCLReduce [] f2 f3 f4 f5 f6 f7
		_ -> original


hasVarName :: [VarName [String]] -> Expr [String] -> Bool
hasVarName loopWrites (Var _ _ list) = foldl (\accum item -> if item then item else accum) False $ map (\(varname, exprs) -> elem varname loopWrites) list
hasVarName loopWrites _ = False


--	Takes two ASTs and appends on onto the other so that the resulting AST is in the correct format
appendFortran_recursive :: Fortran [String] -> Fortran [String] -> Fortran [String]
--appendFortran_recursive newFortran (FSeq _ _ _ (FSeq _ _ _ fortran1)) = appendFortran_recursive newFortran fortran1 
--appendFortran_recursive newFortran (FSeq anno1 src1 fortran1 (FSeq anno2 src2 fortran2 fortran3)) = FSeq anno1 src1 fortran1 (FSeq anno2 src2 fortran2 (appendFortran_recursive newFortran fortran3)) 
appendFortran_recursive newFortran (FSeq anno1 src1 fortran1 (NullStmt anno2 src2)) = FSeq anno1 src1 fortran1 newFortran
appendFortran_recursive newFortran (FSeq anno1 src1 fortran1 fortran2) = FSeq anno1 src1 fortran1 (appendFortran_recursive newFortran fortran2)
appendFortran_recursive newFortran codeSeg = FSeq [] nullSrcSpan codeSeg newFortran

--	Takes an AST and removes the loop statements from the node and joins up the rest of the code so that is it represented in the
--	format that language-fortran uses.
removeLoopConstructs_recursive :: Fortran [String] -> Fortran [String]
removeLoopConstructs_recursive (FSeq anno _ (For _ _ _ _ _ _ fortran1) fortran2) = removeLoopConstructs_recursive $ appendFortran_recursive fortran2 fortran1
removeLoopConstructs_recursive (For _ _ _ _ _ _ fortran) = removeLoopConstructs_recursive fortran
--removeLoopConstructs_recursive (OpenCLMap _ _ _ _ _ fortran1) = removeLoopConstructs_recursive fortran1
--removeLoopConstructs_recursive (OpenCLReduce _ _ _ _ _ _ fortran1) = removeLoopConstructs_recursive fortran1
removeLoopConstructs_recursive (FSeq _ _ fortran (NullStmt _ _)) = removeLoopConstructs_recursive fortran
removeLoopConstructs_recursive codeSeg = codeSeg

getEarliestSrcSpan :: [SrcSpan] -> Maybe(SrcSpan)
getEarliestSrcSpan [] = Nothing
getEarliestSrcSpan spans = Just (foldl (\accum item -> if checkSrcSpanBefore item accum then item else accum) (spans!!0) spans)

checkSrcSpanBefore :: SrcSpan -> SrcSpan -> Bool
checkSrcSpanBefore ((SrcLoc file_before line_before column_before), beforeEnd) ((SrcLoc file_after line_after column_after), afterEnd) = (line_before < line_after) && (column_before < column_after)

checkSrcSpanBefore_line :: SrcSpan -> SrcSpan -> Bool
checkSrcSpanBefore_line ((SrcLoc file_before line_before column_before), beforeEnd) ((SrcLoc file_after line_after column_after), afterEnd) = (line_before < line_after)

getSrcSpanNonIntersection :: SrcSpan -> SrcSpan -> (SrcSpan, SrcSpan)
getSrcSpanNonIntersection src1 src2 = (firstSrc, secondSrc)
					where
						(src1_s, src1_e) = src1
						(src2_s, src2_e) = src2

						firstSrc = (src1_s, src2_s)
						secondSrc = (src2_e, src1_e)

--	Generic function that takes two lists a and b and returns a +list c that is all of the elements of a that do not appear in b.
listSubtract :: Eq a => [a] -> [a] -> [a]
listSubtract a b = filter (\x -> notElem x b) a

listIntersection :: Eq a => [a] -> [a] -> [a]
listIntersection a b = filter (\x -> elem x b) a