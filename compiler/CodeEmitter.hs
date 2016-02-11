module CodeEmitter where

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List
import Data.List.Split
import System.IO
import System.Process

import LanguageFortranTools

emit :: String -> String -> Program [String] -> IO ()
emit filename "" ast = do
				let newFilename = defaultFilename (splitOn "/" filename)
				originalLines <- readOriginalFileLines filename
				let code = produceCodeProg originalLines ast
				--putStr $ show (foldl (++) "" originalLines)
				--writeFile newFilename (show ast)
				--writeFile newFilename (foldl1 (\accum item -> accum ++ "\n" ++ item) originalLines)
				writeFile newFilename code
emit filename specified ast = do		
				originalLines <- readOriginalFileLines filename		
				let code = produceCodeProg originalLines ast
				--writeFile specified (show ast)
				writeFile specified code

defaultFilename :: [String] -> String
defaultFilename (x:[]) = "par_" ++ x
defaultFilename (x:xs) = x ++ "/" ++ defaultFilename xs

readOriginalFileLines :: String -> IO ([String])
readOriginalFileLines filename = do
				--content <- readFile filename
				content <- cpp filename
				let contentLines = lines content
				return contentLines

produceCodeProg :: [String] -> Program [String] -> String
produceCodeProg originalLines prog = foldl (\accum item -> accum ++ produceCodeProgUnit originalLines item) "" prog

produceCodeProgUnit :: [String] -> ProgUnit [String] -> String
produceCodeProgUnit originalLines progUnit =   	nonGeneratedBeforeCode ++
												everything (++) (mkQ "" (produceCodeBlock originalLines)) progUnit
												++ nonGeneratedAfterCode
												-- ++ "\nnonGeneratedBeforeSrc: " ++ show nonGeneratedBeforeSrc
												-- ++ "\nnonGeneratedAfterSrc: " ++ show nonGeneratedAfterSrc
												-- ++ show firstFortranSrc
												
							where
								progUnitSrc = srcSpan progUnit
								firstFortranSrc = (everything (++) (mkQ [] (getFirstFortranSrc)) progUnit)!!0
								(nonGeneratedBeforeSrc, nonGeneratedAfterSrc) = getSrcSpanNonIntersection progUnitSrc firstFortranSrc

								((SrcLoc _ nonGeneratedBefore_ls _), (SrcLoc _ nonGeneratedBefore_le _)) = nonGeneratedBeforeSrc
								nonGeneratedBeforeCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedBefore_ls..nonGeneratedBefore_le-1]

								((SrcLoc _ nonGeneratedAfter_ls _), (SrcLoc _ nonGeneratedAfter_le _)) = nonGeneratedAfterSrc
								nonGeneratedAfterCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedAfter_ls+1..nonGeneratedAfter_le-1]

getFirstFortranSrc :: Block [String] -> [SrcSpan]
getFirstFortranSrc (Block _ _ _ _ _ fortran) = [srcSpan fortran]

produceCodeBlock :: [String] -> Block [String] -> String
produceCodeBlock originalLines block = foldl (++) "" (gmapQ (mkQ "" (produceCode_fortran originalLines)) block)

--produceCode_fortran :: [String] -> Fortran [String] -> String
--produceCode_fortran originalLines (If anno src expr fortran _ _) 	|	f == "generated" = "If (" ++ errorExprFormatting expr ++ ") then\n" 
--																			++ (mkQ "" (produceCode_fortran originalLines) fortran)
--																			++ "end if\n"
--																	|	otherwise = extractOriginalCode originalLines src
--															where 
--																((SrcLoc f lineStart columnStart), (SrcLoc _ lineEnd columnEnd)) = src
--produceCode_fortran originalLines (NullStmt _ _) = ""
--produceCode_fortran originalLines (OpenCLMap _ _ r w l fortran) = "\n\nOpenCLMap (" ++ readArgs ++ writtenArgs ++ generalArgs ++ ")\n{\n"
--																	++ (mkQ "" (produceCode_fortran originalLines) fortran) ++ "\n}\n"
--															where
--																readArgs = foldl (\accum item -> accum ++ "\n\tread_only " ++ (\(VarName _ str) -> str) item) "" (listSubtract r w)
--																writtenArgs = foldl (\accum item -> accum ++ "\n\twrite_only " ++ (\(VarName _ str) -> str) item) "" (listSubtract w r)
--																generalArgs = foldl (\accum item -> accum ++ "\n\t" ++ (\(VarName _ str) -> str) item) "" (listIntersection w r)

--produceCode_fortran originalLines (OpenCLReduce _ _ r w l rv fortran) = "\n\nOpenCLReduce (" ++ readArgs ++ writtenArgs ++ generalArgs ++ ")\n{\n! Reduction vars: " ++ reductionVars ++ "\n"
--																	++ (mkQ "" (produceCode_fortran originalLines) fortran) ++ "\n}\n"
--															where
--																readArgs = foldl (\accum item -> accum ++ "\n\tread_only " ++ (\(VarName _ str) -> str) item) "" (listSubtract r w)
--																writtenArgs = foldl (\accum item -> accum ++ "\n\twrite_only " ++ (\(VarName _ str) -> str) item) "" (listSubtract w r)
--																generalArgs = foldl (\accum item -> accum ++ "\n\t" ++ (\(VarName _ str) -> str) item) "" (listIntersection w r)
--																reductionVars = foldl (\accum item -> accum ++ "\t" ++ (\(VarName _ str) -> str) item) "" rv

--produceCode_fortran originalLines (FSeq _ _ fortran1 fortran2) = (mkQ "" (produceCode_fortran originalLines) fortran1) ++ (mkQ "" (produceCode_fortran originalLines) fortran2)
--produceCode_fortran originalLines codeSeg 	|	anyChildGenerated codeSeg || isGenerated codeSeg = foldl (++) "" (gmapQ (mkQ "" (produceCode_fortran originalLines)) codeSeg)
--											|	otherwise = extractOriginalCode originalLines (srcSpan codeSeg)

produceCode_fortran :: [String] -> Fortran [String] -> String
produceCode_fortran originalLines codeSeg = case codeSeg of
						If _ _ _ _ _ _ -> synthesiseIf originalLines codeSeg
						Assg _ _ _ _ -> synthesiseAssg originalLines codeSeg
						NullStmt _ _ -> ""
						OpenCLMap _ _ _ _ _ _ -> synthesiseOpenCLMap originalLines codeSeg
						OpenCLReduce _ _ _ _ _ _ _ -> synthesiseOpenCLReduce originalLines codeSeg
						FSeq _ _ fortran1 fortran2 -> (mkQ "" (produceCode_fortran originalLines) fortran1) ++ (mkQ "" (produceCode_fortran originalLines) fortran2)
						_ -> 	case anyChildGenerated codeSeg || isGenerated codeSeg of
									True -> foldl (++) "" (gmapQ (mkQ "" (produceCode_fortran originalLines)) codeSeg)
									False -> extractOriginalCode originalLines (srcSpan codeSeg)

synthesiseAssg :: [String] -> Fortran [String] -> String
synthesiseAssg originalLines (Assg anno src expr1 expr2)	|	f == "generated" = errorExprFormatting expr1 ++ " = " ++ errorExprFormatting expr2 ++ "\n"
															|	otherwise = extractOriginalCode originalLines src
											where 
												((SrcLoc f lineStart columnStart), (SrcLoc _ lineEnd columnEnd)) = src

synthesiseIf :: [String] -> Fortran [String] -> String
synthesiseIf originalLines (If anno src expr fortran _ _) 	|	f == "generated" = "If (" ++ errorExprFormatting expr ++ ") then\n" 
																	++ (mkQ "" (produceCode_fortran originalLines) fortran)
																	++ "end if\n"
															|	otherwise = extractOriginalCode originalLines src
											where 
												((SrcLoc f lineStart columnStart), (SrcLoc _ lineEnd columnEnd)) = src

synthesiseOpenCLMap :: [String] -> Fortran [String] -> String
synthesiseOpenCLMap originalLines (OpenCLMap _ _ r w l fortran) = "\n\nOpenCLMap (" ++ readArgs ++ writtenArgs ++ generalArgs ++ ")\n{\n"
																	++ produceCode_fortran originalLines loopInitialiserCode
																	++ (mkQ "" (produceCode_fortran originalLines) fortran) ++ "\n}\n"
											where
												readArgs = foldl (\accum item -> accum ++ "\n\tread_only " ++ (\(VarName _ str) -> str) item) "" (listSubtract r w)
												writtenArgs = foldl (\accum item -> accum ++ "\n\twrite_only " ++ (\(VarName _ str) -> str) item) "" (listSubtract w r)
												generalArgs = foldl (\accum item -> accum ++ "\n\t" ++ (\(VarName _ str) -> str) item) "" (listIntersection w r)
												loopInitialisers = generateLoopInitialisers l (Null [] nullSrcSpan)
												loopInitialiserCode = foldl1 (appendFortran_recursive) loopInitialisers


synthesiseOpenCLReduce :: [String] -> Fortran [String] -> String
synthesiseOpenCLReduce originalLines (OpenCLReduce _ _ r w l rv fortran)  = "\n\nOpenCLReduce (" ++ readArgs ++ writtenArgs ++ generalArgs ++ ")\n{\n! Reduction vars: " ++ reductionVars ++ "\n"
																				++ (mkQ "" (produceCode_fortran originalLines) fortran) ++ "\n}\n"
											where
												readArgs = foldl (\accum item -> accum ++ "\n\tread_only " ++ (\(VarName _ str) -> str) item) "" (listSubtract r w)
												writtenArgs = foldl (\accum item -> accum ++ "\n\twrite_only " ++ (\(VarName _ str) -> str) item) "" (listSubtract w r)
												generalArgs = foldl (\accum item -> accum ++ "\n\t" ++ (\(VarName _ str) -> str) item) "" (listIntersection w r)
												reductionVars = foldl (\accum item -> accum ++ "\t" ++ (\(VarName _ str) -> str) item) "" rv


anyChildGenerated :: Fortran [String] -> Bool
anyChildGenerated ast = everything (||) (mkQ False isGenerated) ast

isGenerated :: Fortran [String] -> Bool
isGenerated codeSeg = f == "generated"
			where
				((SrcLoc f lineStart columnStart), (SrcLoc _ lineEnd columnEnd)) = srcSpan codeSeg

extractOriginalCode :: [String] -> SrcSpan -> String
extractOriginalCode originalLines src = orignalFileChunk
					where 
						((SrcLoc f lineStart columnStart), (SrcLoc _ lineEnd columnEnd)) = src
						orignalFileChunk = foldl (\accum item -> accum ++ (originalLines!!(item-1))++ "\n") "" [lineStart..lineEnd]

generateLoopInitialisers :: [(VarName [String], Expr [String], Expr [String], Expr [String])] -> Expr [String] -> [Fortran [String]]
generateLoopInitialisers ((var, start, end, step):[]) offset = [Assg [] nullSrcSpan (generateVar var)
																	(Bin [] nullSrcSpan (Minus []) globalIdVar offset)]
generateLoopInitialisers ((var, start, end, step):xs) offset = [Assg [] nullSrcSpan (generateVar var)
																	(Bin [] nullSrcSpan (Div []) 
																		(Bin [] nullSrcSpan (Minus []) globalIdVar offset)
																		multipliedExprs)]
																++
																generateLoopInitialisers xs nextOffset
					where
						nextOffset = generateSubtractionExpr ([offset] ++ [generateProductExpr ([generateVar var] ++ followingEndExprs)])
						followingEndExprs = map (\(_,_,_,e) -> e) xs
						multipliedExprs = generateProductExpr followingEndExprs  

generateProductExpr :: [Expr [String]] -> Expr [String]
generateProductExpr (x:[]) = x
generateProductExpr (x:xs) = Bin [] nullSrcSpan (Mul []) x (generateProductExpr xs)

generateSubtractionExpr :: [Expr [String]] -> Expr [String]
generateSubtractionExpr (x:[]) = x
generateSubtractionExpr (x:xs) = Bin [] nullSrcSpan (Minus []) x (generateSubtractionExpr xs)

globalIdVar :: Expr [String]
globalIdVar = Var [] nullSrcSpan [(VarName [] "g_id", [])]

--	Returns an AST representing a set of assignments that determine the values of the loop variables that are being parallelised for a
--	given global_id value.
--flattenLoopConditions :: Maybe (VarName p) -> (VarName p) -> [(VarName p, Expr p, Expr p, Expr p)] -> Fortran p
--flattenLoopConditions prev globalId ((var, start, end, step):[]) = Assg 
--																		(tag globalId) 
--																		nullSrcSpan 
--																		(Var (tag globalId) nullSrcSpan [(var, [])])
--																		(primitiveMod globalId end)
--flattenLoopConditions prev globalId ((var, start, end, step):xs) = 	FSeq 
--																	(tag globalId) 
--																	nullSrcSpan (
--																		Assg 
--																		(tag globalId) 
--																		nullSrcSpan (
--																			Var (tag globalId) nullSrcSpan [(var, [])])
--																		(flattenCondition_div globalId prev (multiplyLoopConditions xs) -- DIVISOR
--																			)
--																		)
--																	 (flattenLoopConditions (Just var) globalId xs) -- FSeq p SrcSpan (Fortran p) (Fortran p) 

--	Function returns an AST represnting a standard division that is performed to calculate loop variable values.
--flattenCondition_div :: VarName p -> Maybe (VarName p) -> Expr p -> Expr p
--flattenCondition_div globalId (Just prev) divisor = Bin 
--														(tag globalId) 
--														nullSrcSpan 
--														(Div (tag globalId)) (
--															Bin 
--																(tag globalId) 
--																nullSrcSpan 
--																(Minus (tag globalId)) 
--																(Var 
--																	(tag globalId) 
--																	nullSrcSpan 
--																	[(globalId, [])]) 
--																(Var 
--																	(tag globalId) 
--																	nullSrcSpan 
--																	[(prev, [])]))
--														divisor
--flattenCondition_div globalId Nothing divisor = 	Bin 
--														(tag globalId) 
--														nullSrcSpan 
--														(Div (tag globalId)) 
--														(Var 
--															(tag globalId) 
--															nullSrcSpan 
--															[(globalId, [])]) 
--														divisor

--	Function returns an AST represnting a standard modulus calculation that is performed to calculate loop variable values.
--flattenCondition_mod :: VarName p -> Maybe (VarName p) -> Expr p -> Expr p
--flattenCondition_mod globalId (Just prev) divisor = Bin 
--														(tag globalId) 
--														nullSrcSpan 
--														(Div (tag globalId)) 
--														(Var 
--															(tag globalId) 
--															nullSrcSpan 
--															[(globalId, [])]) 
--														divisor 

--	Fortran does not have a modulus operator as standard. Therefore, this function returns an AST represnting a modulus calculation
--	that only uses primitive operators (+, -, *, /)
--primitiveMod :: VarName p -> Expr p -> Expr p 
--primitiveMod quotient divisor = Bin 
--									(tag quotient) 
--									nullSrcSpan 
--									(Minus (tag quotient)) 
--									(Var 
--										(tag quotient) 
--										nullSrcSpan 
--										[(quotient, [])]) 
--									(Bin 
--										(tag quotient) 
--										nullSrcSpan 
--										(Mul (tag quotient)) 
--										(Bin 
--											(tag quotient) 
--											nullSrcSpan 
--											(Div (tag quotient)) 
--											(Var 
--												(tag quotient) 
--												nullSrcSpan 
--												[(quotient, [])]) 
--											divisor) 
--										divisor)

-- 	Used by flattenLoopConditions to produce an expression that multiplies together the loop variable dimensions. 
--	This will likely be changed.
--multiplyLoopConditions :: [(VarName p, Expr p, Expr p, Expr p)] -> Expr p
--multiplyLoopConditions ((var, start, end, step):[]) = end
--multiplyLoopConditions ((var, start, end, step):xs) = Bin (tag var) nullSrcSpan (Mul (tag var)) end (multiplyLoopConditions xs)