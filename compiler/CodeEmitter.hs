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
import Data.Maybe

import LanguageFortranTools

--	Code in this file handles the final emission of code. The function 'emit' is called against an AST that has been transformed and has had kernels fused.
--	Trys to make as much use as it can of code that is the same as it was in the original source file. Otherwise, it uses mostly simple functions to generate
--	code segments. Things get more complex when generating reduction kernels as a number of changes to the original source are needed to take full advantage
--	of parallel hardware.

emit :: String -> String -> Program [String] -> IO ()
emit filename specified ast = do
				let newFilename = case specified of
							"" -> defaultFilename (splitOn "/" filename)
							_ -> specified
				let kernelModuleNamePath = generateKernelModuleName (splitOn "/" newFilename)
				let kernelModuleName = getModuleName kernelModuleNamePath
				
				originalLines <- readOriginalFileLines filename
				let code = produceCodeProg originalLines ast
				let kernels = extractKernels originalLines ast
				let kernelModuleHeader = "module " ++ kernelModuleName ++ "\n\n\tcontains\n\n"
				let kernelModuleFooter = "end module " ++ kernelModuleName

				writeFile newFilename code
				writeFile kernelModuleNamePath (kernelModuleHeader ++ kernels ++ kernelModuleFooter)

getModuleName :: String -> String
getModuleName filename = head (splitOn "." (last (splitOn "/" filename)))

defaultFilename :: [String] -> String
defaultFilename (x:[]) = "par_" ++ x
defaultFilename (x:xs) = x ++ "/" ++ defaultFilename xs

generateKernelModuleName :: [String] -> String
generateKernelModuleName (x:[]) = "module_kernels_" ++ x
generateKernelModuleName (x:xs) = x ++ "/" ++ generateKernelModuleName xs

readOriginalFileLines :: String -> IO ([String])
readOriginalFileLines filename = do
				--content <- readFile filename
				content <- cpp filename
				let contentLines = lines content
				return contentLines

extractKernels :: [String] -> Program [String] -> String
extractKernels originalLines prog  = everything (++) (mkQ "" (synthesiseKernels originalLines prog)) prog

synthesiseKernels :: [String] -> Program [String] -> Fortran [String] -> String
synthesiseKernels originalLines prog codeSeg = case codeSeg of
				OpenCLMap _ _ _ _ _ _ -> synthesiseOpenCLMap originalLines prog codeSeg
				OpenCLReduce _ _ _ _ _ _ _ ->  synthesiseOpenCLReduce originalLines prog codeSeg
				_ -> ""

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
						OpenCLMap _ _ _ _ _ _ -> (generateKernelCall codeSeg) -- ++ (synthesiseOpenCLMap originalLines codeSeg)
						OpenCLReduce _ _ _ _ _ _ _ ->  (generateKernelCall codeSeg) -- ++ (synthesiseOpenCLReduce originalLines codeSeg)
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

synthesiseOpenCLMap :: [String] -> Program [String] -> Fortran [String] -> String
synthesiseOpenCLMap originalLines prog (OpenCLMap _ src r w l fortran) = -- "\n! " ++ compilerName ++ ": Synthesised kernel\n" 
																	"subroutine " ++ kernelName
																	++"(" 
																					++ readArgsStr 
																					++ writtenArgsStr 
																					++ generalArgsStr ++ ")\n\n"
																	++ "! " ++ compilerName ++ ": Synthesised loop variables\n"
																	++ readDecls
																	++ produceCode_fortran originalLines loopInitialiserCode 
																	++ "\n\n"
																	++ "! " ++ compilerName ++ ": Original code\n" 
																	++ (mkQ "" (produceCode_fortran originalLines) fortran) 
																	++ "\n"
																	++ "end subroutine " ++ kernelName
																	++ "\n"
																	-- ++ "! " ++ compilerName ++ ": End of synthesised kernel\n\n" 

											where
												kernelName = generateKernelName "map" src w

												readArgs = listSubtract r w
												writtenArgs = listSubtract w r
												generalArgs = listIntersection w r

												readArgsStr = foldl (\accum item -> accum ++ "\n\tread_only " ++ varnameStr item) "" readArgs
												writtenArgsStr = foldl (\accum item -> accum ++ "\n\twrite_only " ++ varnameStr item) "" writtenArgs
												generalArgsStr = foldl (\accum item -> accum ++ "\n\t" ++ varnameStr item) "" generalArgs
												
												readDecls = foldl (\accum item -> accum ++ fromMaybe "Nothing" (getOriginalDeclaration originalLines item prog)) "" readArgs
												-- testDecl = case getOriginalDeclaration originalLines (head readArgs) prog of
												-- 				Just a -> a
												-- 				Nothing -> "Nothing"

												loopInitialisers = generateLoopInitialisers l getGlobalID Nothing
												loopInitialiserCode = foldl1 (\accum item -> appendFortran_recursive item accum) loopInitialisers


synthesiseOpenCLReduce :: [String] -> Program [String] -> Fortran [String] -> String
synthesiseOpenCLReduce originalLines prog (OpenCLReduce _ src r w l rv fortran)  = -- "\n! " ++ compilerName ++ ": Synthesised kernel\n" 
																			"subroutine " ++ kernelName
																			++ "(" 				++ readArgs 
																								++ writtenArgs 
																								++ generalArgs 
																								++ workGroup_reductionArrays_str 
																								++ global_reductionArrays_str 
																								++ chunkSize_str ++ ")\n\n"
																			-- ++ "! " ++ compilerName ++ ": Reduction vars: " ++ global_reductionVars ++ "\n"
																			++ localChunkSize_str
																			++ startPosition_str
																			-- ++ "! " ++ compilerName ++ ": Synthesised loop variables\n"
																			-- ++ produceCode_fortran originalLines local_loopInitialiserCode ++ "\n\n"
																			++ "! " ++ compilerName ++ ": Work item reduction\n" 
																			++ local_reductionVarsInitStr
																			++ "\n"
																			++ (mkQ "" (produceCode_fortran originalLines) workItem_loop)
																			++ "\n"
																			++ workGroup_reductionArraysInitStr
																			++ "\n"
																			++ localMemBarrier
																			++ "\n"
																			++ local_reductionVarsInitStr
																			++ "\n! Workgroup reduction\n"
																			++ (mkQ "" (produceCode_fortran originalLines) workGroup_loop)
																			++ "\n! Write to output array(s) goes here\n"
																			++ global_reductionArraysAssignmentStr
																			++ "\n"
																			++ "end subroutine " ++ kernelName
																			++"\n"
																			-- ++ "! " ++ compilerName ++ ": End of synthesised kernel\n\n" 
											where
												kernelName = generateKernelName "reduce" src (map (\(v, e) -> v) rv)

												reductionVarNames = map (\(varname, expr) -> varname) rv
												readVarNames = listSubtract (listSubtract r w) reductionVarNames
												writtenVarNames = listSubtract (listSubtract w r) reductionVarNames
												generalVarNames = listSubtract (listIntersection w r) reductionVarNames

												readArgs = foldl (\accum item -> accum ++ "\n\t__global read_only " ++ varnameStr item) "" readVarNames
												writtenArgs = foldl (\accum item -> accum ++ "\n\t__global write_only " ++ varnameStr item) "" writtenVarNames
												generalArgs = foldl (\accum item -> accum ++ "\n\t__global " ++ varnameStr item) "" generalVarNames
												
												local_reductionVars = map (generateLocalReductionVar) reductionVarNames
												local_reductionVarsInitStr = foldl (\accum (var, expr) -> accum ++ "local_" ++ varnameStr var ++ " = " ++ errorExprFormatting expr ++ "\n") "" rv

												chunkSize_str = "\n\tconst " ++ errorExprFormatting chunk_size
												localChunkSize_str = (errorExprFormatting localChunkSize) ++ " = " 
														++ (errorExprFormatting chunk_size) ++ " / " ++ (errorExprFormatting getLocalSize) ++ "\n"
												startPosition_str = (errorExprFormatting startPosition) ++ " = " ++ (errorExprFormatting localChunkSize) ++
													" * " ++ (errorExprFormatting getGlobalID) ++ "\n"

												reductionIterator = generateReductionIterator (r ++ w ++ (map (\(x,_,_,_) -> x) l) ++ reductionVarNames)
												workItem_loopEnd = Bin [] nullSrcSpan (Plus []) startPosition localChunkSize
												workItem_loopCode = appendFortran_recursive workItem_reductionCode workItem_loopInitialiserCode 
												workItem_loop = generateLoop reductionIterator startPosition workItem_loopEnd workItem_loopCode
												workItem_reductionCode = applyGeneratedSrcSpans (replaceAllOccurences_varnamePairs fortran reductionVarNames local_reductionVars)

												workItem_loopInitialisers = generateLoopInitialisers l (generateVar reductionIterator) Nothing
												workItem_loopInitialiserCode = foldl1 (\accum item -> appendFortran_recursive item accum) workItem_loopInitialisers

												workGroup_reductionArrays = map (generateLocalReductionArray) reductionVarNames
												workGroup_reductionArrays_str =	foldl (generateLocalReductionArrayArgStr) "" workGroup_reductionArrays
												workGroup_reductionArraysInitStr = foldl (generateReductionArrayAssignment getLocalId) "" (zip workGroup_reductionArrays local_reductionVars)
												workGroup_reductionCode = generateWorkGroupReduction reductionIterator fortran
												workGroup_loop = generateLoop reductionIterator (generateConstant 0) getLocalSize workGroup_reductionCode

												global_reductionArrays = map (generateGlobalReductionArray) reductionVarNames
												global_reductionArrays_str = foldl (generateGloablReductionArrayArgStr) "" global_reductionArrays
												global_reductionArraysAssignmentStr = foldl (generateReductionArrayAssignment getGlobalID) "" (zip global_reductionArrays local_reductionVars)

localChunkSize = generateVar (VarName [] "local_chunk_size")
startPosition  = generateVar (VarName [] "start_position")
chunk_size = generateVar (VarName [] "chunk_size")
localMemBarrier = "barrier(CLK_LOCAL_MEM_FENCE)\n"

generateLocalReductionArray (VarName anno str) = VarName anno ("local_" ++ str ++ "_array")
generateGlobalReductionArray (VarName anno str) = VarName anno ("global_" ++ str ++ "_array")
generateLocalReductionArrayArgStr accum item = accum ++ "\n\t__local " ++ varnameStr item
generateGloablReductionArrayArgStr accum item = accum ++ "\n\t__global " ++ varnameStr item
generateLocalReductionVar (VarName anno str) = VarName anno ("local_" ++ str)

generateReductionArrayAssignment accessor accum ((VarName _ s1),(VarName _ s2)) = accum++s1++"("++(errorExprFormatting accessor)++") = "++s2++"\n"

generateKernelName :: String -> SrcSpan -> [VarName [String]] -> String
generateKernelName identifier src varnames = identifier
											++ (foldl (\accum item -> accum ++ "_" ++ (varnameStr item)) "" varnames) 
											++ "_" ++ show (extractLineNumber src)

generateKernelCall :: Fortran [String] -> String
generateKernelCall (OpenCLMap _ src r w l fortran) = 	"call " ++ (generateKernelName "map" src w) 
														++ "(" ++ allArgumentsStr ++ ")"++ "\t! Call to synthesised, external kernel\n"++ "\n"
			where
				readArgs = map (varnameStr) (listSubtract r w)
				writtenArgs = map (varnameStr) (listSubtract w r)
				generalArgs = map (varnameStr) (listIntersection w r)

				allArguments = readArgs ++ writtenArgs ++ generalArgs
				allArgumentsStr =  (head allArguments) ++ foldl (\accum item -> accum ++ "," ++ item) "" (tail allArguments)
generateKernelCall (OpenCLReduce _ src r w l rv fortran) = 	"call " ++ (generateKernelName "reduce" src (map (\(v, e) -> v) rv)) 
															++ "(" ++ allArgumentsStr ++ ")" ++ "\t! Call to synthesised, external kernel\n"++ "\n"
			where 

				reductionVarNames = map (\(varname, expr) -> varname) rv
				workGroup_reductionArrays = map (generateLocalReductionArray) reductionVarNames
				global_reductionArrays = map (generateGlobalReductionArray) reductionVarNames
				readArgs = listSubtract r w
				writtenArgs = listSubtract w r
				generalArgs = listIntersection w r

				allArguments = 	(map (varnameStr) 
									(listSubtract 
										(readArgs ++ writtenArgs ++ generalArgs ++ workGroup_reductionArrays ++ global_reductionArrays) 
										reductionVarNames)) 
								++ ["chunk_size"]
				allArgumentsStr =  (head allArguments) ++ foldl (\accum item -> accum ++ "," ++ item) "" (tail allArguments)

getOriginalDeclaration :: [String] -> VarName [String] -> Program [String] -> Maybe(String)
getOriginalDeclaration originalLines varname program = case declSrc_list of
														[] -> Nothing
														_ -> Just (extractOriginalCode originalLines declSrc)
			where 
				declSrc_list = everything (++) (mkQ [] (extractDeclarationSrcSpan varname)) program
				declSrc = head declSrc_list


extractDeclarationSrcSpan :: VarName [String] -> Decl [String] -> [SrcSpan]
extractDeclarationSrcSpan varname (Decl _ src lst _) 	| firstHasVar || secondHasVar = [src]
														| otherwise = []
			where
				firstExprs = map (\(expr, _, _) -> expr) lst
				secondExprs = map (\(_, expr, _) -> expr) lst

				firstHasVar = foldl (\accum item -> accum || hasVarName [varname] item) False firstExprs
				secondHasVar = foldl (\accum item -> accum || hasVarName [varname] item) False secondExprs
extractDeclarationSrcSpan varname decl = []


anyChildGenerated :: Fortran [String] -> Bool
anyChildGenerated ast = everything (||) (mkQ False isGenerated) ast

isGenerated :: Fortran [String] -> Bool
isGenerated codeSeg = f == "generated"
			where
				((SrcLoc f lineStart columnStart), (SrcLoc _ lineEnd columnEnd)) = srcSpan codeSeg

--	Function takes a list of lines from the original source and an object representing a range of line numbers and reproduces the original code
--	in the range of those line numbers.
extractOriginalCode :: [String] -> SrcSpan -> String
extractOriginalCode originalLines src = orignalFileChunk
					where 
						((SrcLoc f lineStart columnStart), (SrcLoc _ lineEnd columnEnd)) = src
						orignalFileChunk = foldl (\accum item -> accum ++ (originalLines!!(item-1))++ "\n") "" [lineStart..lineEnd]

generateLoop :: VarName [String] -> Expr [String] -> Expr [String] -> Fortran[String] -> Fortran[String]
generateLoop r_iter start end fortran = For [] nullSrcSpan r_iter start end step fortran
					where
						step = Con [] nullSrcSpan "1"

generateWorkGroupReduction :: VarName [String] -> Fortran [String] -> Fortran [String]
generateWorkGroupReduction redIter codeSeg  = resultantCode
					where
						assignments = everything (++) (mkQ [] (generateWorkGroupReduction_assgs redIter)) codeSeg
						resultantCode = foldl1 (\accum item -> appendFortran_recursive item accum) assignments

generateWorkGroupReduction_assgs :: VarName [String] -> Fortran [String] -> [Fortran [String]]
generateWorkGroupReduction_assgs redIter (Assg _ _ expr1 expr2) = resultantAssg
					where 
						resultantAssg = case extractPrimaryReductionOp expr1 expr2 of
											Just op -> [Assg [] nullSrcSpan localReductioVar (Bin [] nullSrcSpan op localReductioVar localReductionArray)]
											Nothing -> case extractPrimaryReductionFunction expr1 expr2 of
														"" -> []
														funcName -> [Assg [] nullSrcSpan localReductioVar (Var [] nullSrcSpan [(VarName [] funcName, [localReductioVar, localReductionArray])])]
						localReductionArray = generateArrayVar (generateLocalReductionArray (head (extractVarNames expr1))) (generateVar redIter)
						localReductioVar = generateVar (generateLocalReductionVar (head (extractVarNames expr1)))
generateWorkGroupReduction_assgs redIter codeSeg = []

generateLoopInitialisers :: [(VarName [String], Expr [String], Expr [String], Expr [String])] -> Expr [String] -> Maybe(Expr [String]) -> [Fortran [String]]
generateLoopInitialisers ((var, start, end, step):[]) iterator (Just offset) 
			= 	[Assg [] nullSrcSpan 
				(generateVar var)
				(offset)]

generateLoopInitialisers ((var, start, end, step):xs) iterator Nothing 
			= 	[Assg [] nullSrcSpan 
				(generateVar var)
				(Bin [] nullSrcSpan (Div [])  iterator multipliedExprs)]
				++
				generateLoopInitialisers xs iterator (Just nextOffset)
					where
						--nextOffset = generateSubtractionExpr ([generateProductExpr ([generateVar var] ++ followingEndExprs)])
						nextOffset = generateSubtractionExpr ([iterator] ++ [generateProductExpr ([generateVar var] ++ followingEndExprs)])
						followingEndExprs = map (\(_,_,e,_) -> e) xs
						multipliedExprs = generateProductExpr followingEndExprs 
generateLoopInitialisers ((var, start, end, step):xs) iterator (Just offset) 
			= 	[Assg [] nullSrcSpan (generateVar var)
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