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

produceCode_fortran :: [String] -> Fortran [String] -> String
produceCode_fortran originalLines codeSeg = case codeSeg of
						If _ _ _ _ _ _ -> synthesiseIf originalLines codeSeg
						Assg _ _ _ _ -> synthesiseAssg originalLines codeSeg
						For _ _ _ _ _ _ _ -> synthesiseFor originalLines codeSeg
						NullStmt _ _ -> ""
						OpenCLMap _ _ _ _ _ _ -> synthesiseOpenCLMap originalLines codeSeg
						OpenCLReduce _ _ _ _ _ _ _ -> synthesiseOpenCLReduce originalLines codeSeg
						FSeq _ _ fortran1 fortran2 -> (mkQ "" (produceCode_fortran originalLines) fortran1) ++ (mkQ "" (produceCode_fortran originalLines) fortran2)
						_ -> 	case anyChildGenerated codeSeg || isGenerated codeSeg of
									True -> foldl (++) "" (gmapQ (mkQ "" (produceCode_fortran originalLines)) codeSeg)
									False -> extractOriginalCode originalLines (srcSpan codeSeg)

synthesiseFor :: [String] -> Fortran [String] -> String
synthesiseFor originalLines (For anno src varname expr1 expr2 expr3 fort) 	|	f == "generated" = "\tdo " ++ (varnameStr varname) ++ "=" ++ errorExprFormatting expr1 
																								++ ", " ++ errorExprFormatting expr2 ++ (if expr3isOne then "" else errorExprFormatting expr3)
																								++ "\n" ++ (mkQ "" (produceCode_fortran originalLines) fort) ++ "\tend do\n"
																			|	otherwise = extractOriginalCode originalLines src
																	where
																		expr3isOne = case expr3 of
																						Con _ _ "1" -> True
																						_ -> False
																		((SrcLoc f lineStart columnStart), (SrcLoc _ lineEnd columnEnd)) = src

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
synthesiseOpenCLMap originalLines (OpenCLMap _ _ r w l fortran) = "\n! " ++ compilerName ++ ": Synthesised kernel\n" 
																	++ "OpenCLMap (" ++ readArgs ++ writtenArgs ++ generalArgs ++ ")\n{\n"
																	++ "! " ++ compilerName ++ ": Synthesised loop variables\n"
																	++ produceCode_fortran originalLines loopInitialiserCode ++ "\n\n"
																	++ "! " ++ compilerName ++ ": Original code\n" 
																	++ (mkQ "" (produceCode_fortran originalLines) fortran) ++ "\n}\n"
																	-- ++ "! " ++ compilerName ++ ": End of synthesised kernel\n\n" 

											where
												readArgs = foldl (\accum item -> accum ++ "\n\tread_only " ++ varnameStr item) "" (listSubtract r w)
												writtenArgs = foldl (\accum item -> accum ++ "\n\twrite_only " ++ varnameStr item) "" (listSubtract w r)
												generalArgs = foldl (\accum item -> accum ++ "\n\t" ++ varnameStr item) "" (listIntersection w r)
												loopInitialisers = generateLoopInitialisers l getGlobalID Nothing
												loopInitialiserCode = foldl1 (\accum item -> appendFortran_recursive item accum) loopInitialisers


synthesiseOpenCLReduce :: [String] -> Fortran [String] -> String
synthesiseOpenCLReduce originalLines (OpenCLReduce _ _ r w l rv fortran)  = "\n! " ++ compilerName ++ ": Synthesised kernel\n" 
																			++ "OpenCLReduce (" ++ readArgs ++ writtenArgs ++ generalArgs ++ workGroup_reductionArrays_str ++ chunkSize_str ++ ")\n{\n"
																			-- ++ "! " ++ compilerName ++ ": Reduction vars: " ++ global_reductionVars ++ "\n"
																			++ localChunkSize_str
																			++ startPosition_str
																			-- ++ "! " ++ compilerName ++ ": Synthesised loop variables\n"
																			-- ++ produceCode_fortran originalLines local_loopInitialiserCode ++ "\n\n"
																			++ "! " ++ compilerName ++ ": Local reduction\n" 
																			++ local_reductionVarsInitStr
																			++ "\n"
																			++ (mkQ "" (produceCode_fortran originalLines) workItem_Loop)
																			++ "\n"
																			++ workGroup_reductionArraysInitStr
																			++ "\n"
																			++ localMemBarrier
																			++ "\n"
																			++ local_reductionVarsInitStr
																			++ "! Workgroup reduction goes here\n"
																			++ "! Write to output array(s) goes here\n"
																			++ "\n}\n"
																			-- ++ "! " ++ compilerName ++ ": End of synthesised kernel\n\n" 
											where
												readArgs = foldl (\accum item -> accum ++ "\n\t__global read_only " ++ varnameStr item) "" (listSubtract r w)
												writtenArgs = foldl (\accum item -> accum ++ "\n\t__global write_only " ++ varnameStr item) "" (listSubtract w r)
												generalArgs = foldl (\accum item -> accum ++ "\n\t__global " ++ varnameStr item) "" (listIntersection w r)
												-- reductionVars = foldl (\accum item -> accum ++ "\t" ++ varnameStr item) "" rv
												reductionVarNames = map (\(varname, expr) -> varname) rv
												
												local_reductionVars = map (\(VarName anno str) -> VarName anno ("local_" ++ str)) reductionVarNames
												-- local_reductionVarsInitStr = foldl (\accum item -> accum ++ "! (incorrect) local_" ++ varnameStr item ++ " = " ++ varnameStr item ++ "(" ++ (errorExprFormatting startPosition) ++ ")\n") "" reductionVarNames
												local_reductionVarsInitStr = foldl (\accum (var, expr) -> accum ++ "local_" ++ varnameStr var ++ " = " ++ errorExprFormatting expr ++ "\n") "" rv


												chunkSize_str = "\n\tconst " ++ errorExprFormatting chunk_size
												localChunkSize_str = (errorExprFormatting localChunkSize) ++ " = " 
														++ (errorExprFormatting chunk_size) ++ " / " ++ (errorExprFormatting getLocalSize) ++ "\n"
												-- localChunkSize_str = "local_chunk_size = chunk_size / " ++ errorExprFormatting getLocalSize ++ "\n"
												startPosition_str = (errorExprFormatting startPosition) ++ " = " ++ (errorExprFormatting localChunkSize) ++
													" * " ++ (errorExprFormatting getGlobalID) ++ "\n"
												-- startPosition_str = "start_position = local_chunk_size * " ++ errorExprFormatting getGlobalID ++ "\n"


												reductionIterator = generateReductionIterator (r ++ w ++ (map (\(x,_,_,_) -> x) l) ++ reductionVarNames)
												workItem_LoopEnd = Bin [] nullSrcSpan (Plus []) startPosition localChunkSize
												workItem_LoopCode = appendFortran_recursive workItem_reductionCode workItem_loopInitialiserCode 
												workItem_Loop = generateLoop reductionIterator startPosition workItem_LoopEnd workItem_LoopCode
												workItem_reductionCode = applyGeneratedSrcSpans (replaceAllOccurences_varnamePairs fortran reductionVarNames local_reductionVars)

												workItem_loopInitialisers = generateLoopInitialisers l (generateVar reductionIterator) Nothing
												workItem_loopInitialiserCode = foldl1 (\accum item -> appendFortran_recursive item accum) workItem_loopInitialisers

												workGroup_reductionArrays = map (\(VarName anno str) -> VarName anno ("local_" ++ str ++ "_array")) reductionVarNames
												workGroup_reductionArrays_str =	foldl (\accum item -> accum ++ "\n\t__local " ++ varnameStr item) "" workGroup_reductionArrays
												workGroup_reductionArraysInitStr = foldl (\accum ((VarName _ s1),(VarName _ s2)) -> accum++s1++"("++(errorExprFormatting getLocalId)++") = "++s2++"\n") "" (zip workGroup_reductionArrays local_reductionVars)

localChunkSize = generateVar (VarName [] "local_chunk_size")
startPosition  = generateVar (VarName [] "start_position")
chunk_size = generateVar (VarName [] "chunk_size")
localMemBarrier = "barrier(CLK_LOCAL_MEM_FENCE)\n"

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

generateLoop :: VarName [String] -> Expr [String] -> Expr [String] -> Fortran[String] -> Fortran[String]
generateLoop r_iter start end fortran = For [] nullSrcSpan r_iter start end step fortran
					where
						step = Con [] nullSrcSpan "1"

generateLoopInitialisers :: [(VarName [String], Expr [String], Expr [String], Expr [String])] -> Expr [String] -> Maybe(Expr [String]) -> [Fortran [String]]
generateLoopInitialisers ((var, start, end, step):[]) iterator (Just offset) = [Assg [] nullSrcSpan 
																		(generateVar var)
																		(offset)]

generateLoopInitialisers ((var, start, end, step):xs) iterator Nothing = [Assg [] nullSrcSpan 
																	(generateVar var)
																	(Bin [] nullSrcSpan (Div [])  iterator multipliedExprs)]
																++
																generateLoopInitialisers xs iterator (Just nextOffset)
					where
						--nextOffset = generateSubtractionExpr ([generateProductExpr ([generateVar var] ++ followingEndExprs)])
						nextOffset = generateSubtractionExpr ([iterator] ++ [generateProductExpr ([generateVar var] ++ followingEndExprs)])
						followingEndExprs = map (\(_,_,e,_) -> e) xs
						multipliedExprs = generateProductExpr followingEndExprs  
generateLoopInitialisers ((var, start, end, step):xs) iterator (Just offset) = [Assg [] nullSrcSpan (generateVar var)
																	(Bin [] nullSrcSpan (Div []) 
																		offset
																		multipliedExprs)]
																++
																generateLoopInitialisers xs iterator (Just nextOffset)
					where
						nextOffset = generateSubtractionExpr ([offset] ++ [generateProductExpr ([generateVar var] ++ followingEndExprs)])
						followingEndExprs = map (\(_,_,e,_) -> e) xs
						multipliedExprs = generateProductExpr followingEndExprs  

generateProductExpr :: [Expr [String]] -> Expr [String]
generateProductExpr (x:[]) = x
generateProductExpr (x:xs) = Bin [] nullSrcSpan (Mul []) x (generateProductExpr xs)

generateSubtractionExpr :: [Expr [String]] -> Expr [String]
generateSubtractionExpr (x:[]) = x
generateSubtractionExpr (x:xs) = Bin [] nullSrcSpan (Minus []) x (generateSubtractionExpr xs)

getGlobalID :: Expr [String]
getGlobalID = Var [] nullSrcSpan [(VarName [] "g_id", [])]

getLocalSize :: Expr [String]
getLocalSize = Var [] nullSrcSpan [(VarName [] "get_local_size", [Con [] nullSrcSpan "0"])]

getLocalId :: Expr [String]
getLocalId = Var [] nullSrcSpan [(VarName [] "get_local_id", [Con [] nullSrcSpan "0"])]

generateReductionIterator :: [VarName [String]] -> VarName [String]
generateReductionIterator usedNames = VarName [] choice
			where
				possibles = ["reduction_iterator", "reduction_iter", "r_iter"]
				choice = foldl1 (\accum item -> if not (elem (VarName [] item) usedNames) then item else accum) possibles