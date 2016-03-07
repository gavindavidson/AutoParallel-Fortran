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
				let originalListing = case originalLines of
										[]	-> ""
										_ -> foldl (\accum item -> accum ++ "\n" ++ item) (head originalLines) (tail originalLines)
				let originalFileName = generateOriginalFileName (splitOn "/" newFilename)

				let code = produceCodeProg originalLines ast
				let kernels = extractKernels originalLines ast
				let kernelModuleHeader = "module " ++ kernelModuleName ++ "\n\n\tcontains\n\n"
				let kernelModuleFooter = "end module " ++ kernelModuleName

				writeFile originalFileName originalListing
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

generateOriginalFileName :: [String] -> String
generateOriginalFileName (x:[]) = "original_" ++ x
generateOriginalFileName (x:xs) = x ++ "/" ++ generateOriginalFileName xs

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
				OpenCLMap _ _ _ _ _ _ -> synthesiseOpenCLMap "" originalLines prog codeSeg
				OpenCLReduce _ _ _ _ _ _ _ ->  synthesiseOpenCLReduce "" originalLines prog codeSeg
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
								firstFortranSrc = head (everything (++) (mkQ [] (getFirstFortranSrc)) progUnit)
								(nonGeneratedBeforeSrc, nonGeneratedAfterSrc) = getSrcSpanNonIntersection progUnitSrc firstFortranSrc

								((SrcLoc _ nonGeneratedBefore_ls _), (SrcLoc _ nonGeneratedBefore_le _)) = nonGeneratedBeforeSrc
								nonGeneratedBeforeCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedBefore_ls..nonGeneratedBefore_le-1]

								((SrcLoc _ nonGeneratedAfter_ls _), (SrcLoc _ nonGeneratedAfter_le _)) = nonGeneratedAfterSrc
								nonGeneratedAfterCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedAfter_ls+1..nonGeneratedAfter_le-1]

getFirstFortranSrc :: Block [String] -> [SrcSpan]
getFirstFortranSrc (Block _ _ _ _ _ fortran) = [srcSpan fortran]

produceCodeBlock :: [String] -> Block [String] -> String
produceCodeBlock originalLines block = foldl (++) "" (gmapQ (mkQ "" (produceCode_fortran "" originalLines)) block)

produceCode_fortran :: String -> [String] -> Fortran [String] -> String
produceCode_fortran tabs originalLines codeSeg = case codeSeg of
						If _ _ _ _ _ _ -> synthesiseIf tabs originalLines codeSeg
						Assg _ _ _ _ -> synthesiseAssg tabs originalLines codeSeg
						For _ _ _ _ _ _ _ -> synthesiseFor tabs originalLines codeSeg
						NullStmt _ _ -> ""
						OpenCLMap _ _ _ _ _ _ -> (generateKernelCall codeSeg) 
						OpenCLReduce _ _ _ _ _ rv f ->  (generateKernelCall codeSeg) ++ "! Replace n with number of work groups\n " 
																++ (mkQ "" (produceCode_fortran "" originalLines) hostReductionLoop) ++ "\n"
								where 
									reductionVarNames = map (\(varname, expr) -> varname) rv
									r_iter = generateReductionIterator reductionVarNames
									hostReduction = generateFinalHostReduction reductionVarNames r_iter f
									hostReductionLoop = generateLoop r_iter (generateConstant 1) (generateVar (VarName [] "n")) hostReduction
									
						FSeq _ _ fortran1 fortran2 -> (mkQ "" (produceCode_fortran tabs originalLines) fortran1) ++ (mkQ "" (produceCode_fortran tabs originalLines) fortran2)
						_ -> 	case anyChildGenerated codeSeg || isGenerated codeSeg of
									True -> foldl (++) tabs (gmapQ (mkQ "" (produceCode_fortran "" originalLines)) codeSeg)
									False -> extractOriginalCode tabs originalLines (srcSpan codeSeg)

synthesiseFor :: String -> [String] -> Fortran [String] -> String
synthesiseFor tabs originalLines (For anno src varname expr1 expr2 expr3 fort) 	|	f == "generated" = tabs ++ "do " ++ (varnameStr varname) ++ "=" ++ outputExprFormatting expr1 
																								++ ", " ++ outputExprFormatting expr2 ++ (if expr3isOne then "" else outputExprFormatting expr3)
																								++ "\n" ++ (mkQ "" (produceCode_fortran (tabs ++ tabInc) originalLines) fort) ++ tabs ++ "end do\n"
																			|	otherwise = extractOriginalCode tabs originalLines src
																	where
																		expr3isOne = case expr3 of
																						Con _ _ "1" -> True
																						_ -> False
																		((SrcLoc f lineStart columnStart), (SrcLoc _ lineEnd columnEnd)) = src

synthesiseAssg :: String -> [String] -> Fortran [String] -> String
synthesiseAssg tabs originalLines (Assg anno src expr1 expr2)	|	f == "generated" = tabs ++ outputExprFormatting expr1 ++ " = " ++ outputExprFormatting expr2 ++ "\n"
															|	otherwise = extractOriginalCode tabs originalLines src
											where 
												((SrcLoc f lineStart columnStart), (SrcLoc _ lineEnd columnEnd)) = src

synthesiseIf :: String -> [String] -> Fortran [String] -> String
synthesiseIf tabs originalLines (If anno src expr fortran _ _) 	|	f == "generated" = tabs ++ "If (" ++ outputExprFormatting expr ++ ") then\n" 
																	++ (mkQ "" (produceCode_fortran (tabs ++ tabInc) originalLines) fortran)
																	++ tabs ++ "end if\n"
															|	otherwise = extractOriginalCode tabs originalLines src
											where 
												((SrcLoc f lineStart columnStart), (SrcLoc _ lineEnd columnEnd)) = src

synthesiseDecl :: String -> Decl [String] -> String
synthesiseDecl tabs (Decl anno src lst typ) = tabs ++ (synthesiseType typ) ++ " :: " ++ (synthesiseDeclList lst) ++ "\n"
synthesiseDecl tabs (NullDecl [] nullSrcSpan) = tabs ++ "[Variable not declared in orignal source]\n"
synthesiseDecl tabs _ = tabs ++ "[Unimplemented declaration syntax] \n"

synthesiseDecls :: String -> [Decl [String]] -> String
synthesiseDecls tabs decls = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" decls

synthesiseType :: Type [String] -> String
synthesiseType (BaseType anno base attrList (expr1) (expr2)) = baseStr ++ kindStr ++ attrStr
											where
												baseStr = synthesiseBaseType base
												kindStr = case outputExprFormatting expr1 of
																"" -> ""
																str -> "(kind=" ++ str ++ ")"
												attrStr = case synthesiseAttrList attrList of
																"" -> ""
																str -> ", " ++ str

synthesiseBaseType :: BaseType [String] -> String
synthesiseBaseType (Integer _) = "integer"
synthesiseBaseType (Real _) = "real"
synthesiseBaseType (Character _) = "character"
synthesiseBaseType typ = "[Incompatible type]"
-- synthesiseBaseType (SomeType _) = "<SomeType>"
-- synthesiseBaseType (DerivedType _ _) = "<DerivedType>"
-- synthesiseBaseType (Recursive _) = "<Recursive>"
-- synthesiseBaseType (Pure _) = "<Pure>"
-- synthesiseBaseType (Elemental _) = "<Elemental>"
-- synthesiseBaseType (Logical _) = "<Logical>"
-- synthesiseBaseType (Complex _) = "<Complex>"

synthesiseAttrList :: [Attr [String]] -> String
synthesiseAttrList [] = ""
synthesiseAttrList (attr:[]) = synthesiseAttr attr
synthesiseAttrList attrList = attrStrs
				where 
					attrStrList = map (synthesiseAttr) attrList
					attrStrs = foldl (\accum item -> accum ++ ", " ++ item) (head attrStrList) (tail attrStrList)

synthesiseAttr :: Attr [String] -> String
synthesiseAttr (Dimension _ exprList) = "Dimension(" ++ synthesiseRangeExpr exprList ++ ")"
synthesiseAttr (Intent _ intentAttr) = "Intent(" ++ intentStr ++ ")"
					where 
						intentStr = case intentAttr of
							In _ -> "In"
							Out _ -> "Out"
							_ -> "InOut"
synthesiseAttr attr = "[Incompatible attribute]"
-- synthesiseAttr (Parameter _) = ""
-- synthesiseAttr (Allocatable _) = ""
-- synthesiseAttr (External _) = ""
-- synthesiseAttr (Intrinsic _) = ""
-- synthesiseAttr (Optional _) = ""
-- synthesiseAttr (Pointer _) = ""
-- synthesiseAttr (Save _) = ""
-- synthesiseAttr (Target _) = ""
-- synthesiseAttr (Volatile _) = ""
-- synthesiseAttr (Public _) = ""
-- synthesiseAttr (Private _) = ""
-- synthesiseAttr (Sequence _) = ""
-- synthesiseAttr (MeasureUnit _ _) = ""

synthesiseRangeExpr :: [(Expr [String], Expr [String])] -> String
synthesiseRangeExpr [] = ""
synthesiseRangeExpr ((expr1, expr2):[]) = outputExprFormatting expr1 ++ ":" ++ outputExprFormatting expr2
synthesiseRangeExpr ((expr1, expr2):xs) = outputExprFormatting expr1 ++ ":" ++ outputExprFormatting expr2 ++ "," ++ synthesiseRangeExpr xs

synthesiseDeclList :: [(Expr [String], Expr [String], Maybe Int)] -> String
synthesiseDeclList ((expr1, expr2, maybeInt):xs) = "(" 
												++ outputExprFormatting expr1 ++ 
												(case expr2 of
													NullExpr _ _ -> ""
													_ ->  ", " ++ outputExprFormatting expr2)
												++ maybeIntStr ++ ")"
												++ followingItems
						where
							maybeIntStr = case maybeInt of
											Just a -> ", " ++ show a
											Nothing -> ""
							followingItems = case xs of 
												[] -> ""
												_ -> ", " ++ synthesiseDeclList xs

synthesiseOpenCLMap :: String -> [String] -> Program [String] -> Fortran [String] -> String
synthesiseOpenCLMap inTabs originalLines prog (OpenCLMap _ src r w l fortran) = -- "\n! " ++ compilerName ++ ": Synthesised kernel\n" 
																	inTabs ++ "subroutine " ++ kernelName
																	++"(" 
																					++ allArgsStr ++ ")\n\n"
																	++ readDeclStr
																	++ writtenDeclStr
																	++ generalDeclStr
																	++ "\n"
																	++ tabs ++ "! " ++ compilerName ++ ": Synthesised loop variables\n"
																	++ produceCode_fortran (tabs) originalLines loopInitialiserCode 
																	++ "\n\n"
																	++ tabs ++ "! " ++ compilerName ++ ": Original code\n" 
																	++ (mkQ "" (produceCode_fortran (tabs) originalLines) fortran) 
																	++ "\n"
																	++ inTabs ++ "end subroutine " ++ kernelName
																	++ "\n\n\n"
																	-- ++ "! " ++ compilerName ++ ": End of synthesised kernel\n\n" 

											where
												kernelName = generateKernelName "map" src w
												globalIdVar = generateVar (VarName [] "global_id")
												tabs = inTabs ++ tabInc

												readArgs = listSubtract r w
												writtenArgs = listSubtract w r
												generalArgs = listIntersection w r

												allArgs = readArgs ++ writtenArgs ++ generalArgs
												allArgsStr = case allArgs of
															[] -> ""
															args -> foldl (\accum item -> accum ++ "," ++ varnameStr item) (varnameStr (head args)) (tail args)
												
												
												readDecls = map (\x ->fromMaybe (NullDecl [] nullSrcSpan) (adaptOriginalDeclaration x (In []) prog)) readArgs
												writtenDecls = map (\x ->fromMaybe (NullDecl [] nullSrcSpan) (adaptOriginalDeclaration x (Out []) prog)) writtenArgs
												generalDecls = map (\x ->fromMaybe (NullDecl [] nullSrcSpan) (adaptOriginalDeclaration x (InOut []) prog)) generalArgs

												readDeclStr = foldl (\accum item -> accum ++ synthesiseDecl (tabs) item) "" (readDecls)
												writtenDeclStr = foldl (\accum item -> accum ++ synthesiseDecl (tabs) item) "" (writtenDecls)
												generalDeclStr = foldl (\accum item -> accum ++ synthesiseDecl (tabs) item) "" (generalDecls)

												loopInitialisers = generateLoopInitialisers l globalIdVar Nothing
												loopInitialiserCode = foldl1 (\accum item -> appendFortran_recursive item accum) loopInitialisers


synthesiseOpenCLReduce :: String ->  [String] -> Program [String] -> Fortran [String] -> String
synthesiseOpenCLReduce inTabs originalLines prog (OpenCLReduce _ src r w l rv fortran)  = -- "\n! " ++ compilerName ++ ": Synthesised kernel\n" 
																			inTabs ++ "subroutine " ++ kernelName
																			++ "(" 				++ allArgsStr
																								-- ++ workGroup_reductionArrays_str 
																								-- ++ "workGroup_reductionArrays_str"
																								-- ++ global_reductionArrays_str 
																								-- ++ chunkSize_str 
																								++ ")\n\n"
																			++ tabs ++ chunk_sizeDeclaration
																			++ tabs ++ localSizeDeclaration 
																			++ tabs ++ localIdDeclaration
																			++ tabs ++ groupIdDeclaration
																			++ tabs ++ numGroupsDeclaration
																			++ tabs ++ groupSizeDeclaration
																			++ tabs ++ globalIdDeclaration 

																			++ tabs ++ localSizeInitialisation
																			++ tabs ++ localIdInitialisation
																			++ tabs ++ groupIdInitialisation
																			++ tabs ++ numGroupsInitialisation
																			++ tabs ++ groupSizeInitialisation
																			++ tabs ++ globalIdInitialisation
																			++ "\n"
																			++ readDeclStr
																			++ writtenDeclStr
																			++ generalDeclStr
																			++ "\n"
																			++ workGroup_reductionArraysDeclStr
																			++ global_reductionArraysDeclStr
																			++ "\n"
																			-- ++ "! " ++ compilerName ++ ": Reduction vars: " ++ global_reductionVars ++ "\n"
																			++ tabs ++ localChunkSize_str
																			++ tabs ++ startPosition_str
																			-- ++ "! " ++ compilerName ++ ": Synthesised loop variables\n"
																			-- ++ produceCode_fortran originalLines local_loopInitialiserCode ++ "\n\n"
																			++ tabs ++ "! " ++ compilerName ++ ": Work item reduction\n" 
																			++ local_reductionVarsInitStr
																			++ "\n"
																			++ (mkQ "" (produceCode_fortran (tabs) originalLines) workItem_loop)
																			++ "\n"
																			++ workGroup_reductionArraysInitStr
																			++ "\n"
																			++ tabs ++ localMemBarrier
																			++ "\n"
																			++ local_reductionVarsInitStr
																			++ "\n! Workgroup reduction\n"
																			++ (mkQ "" (produceCode_fortran (tabs) originalLines) workGroup_loop)
																			++ "\n! Write to output array(s) goes here\n"
																			++ global_reductionArraysAssignmentStr
																			++ "\n"
																			++ inTabs ++ "end subroutine " ++ kernelName
																			++"\n\n\n"
																			-- ++ "! " ++ compilerName ++ ": End of synthesised kernel\n\n" 
											where
												kernelName = generateKernelName "reduce" src (map (\(v, e) -> v) rv)
												tabs = inTabs ++ tabInc

												reductionVarNames = map (\(varname, expr) -> varname) rv
												readVarNames = listSubtract (listSubtract r w) reductionVarNames
												writtenVarNames = listSubtract (listSubtract w r) reductionVarNames
												generalVarNames = listSubtract (listIntersection w r) reductionVarNames

												localSizeVar = generateVar (VarName [] "local_size")
												localIdVar = generateVar (VarName [] "local_id")
												groupIdVar = generateVar (VarName [] "group_id")
												numGroupsVar = generateVar (VarName [] "num_groups")
												groupSizeVar = generateVar (VarName [] "group_size")
												globalIdVar = generateVar (VarName [] "global_id")

												localSizeDeclaration = "integer :: " ++ outputExprFormatting localSizeVar ++ "\n"
												localIdDeclaration = "integer :: " ++ outputExprFormatting localIdVar ++ "\n"
												groupIdDeclaration = "integer :: " ++ outputExprFormatting groupIdVar ++ "\n"
												numGroupsDeclaration = "integer :: " ++ outputExprFormatting numGroupsVar ++ "\n"
												groupSizeDeclaration = "integer :: " ++ outputExprFormatting groupSizeVar ++ "\n"
												globalIdDeclaration = "integer :: " ++ outputExprFormatting globalIdVar ++ "\n"

												localSizeInitialisation = "call " ++ outputExprFormatting (getLocalSize localSizeVar) ++ "\n"
												localIdInitialisation = "call " ++ outputExprFormatting (getLocalId localIdVar) ++ "\n"
												groupIdInitialisation = "call " ++ outputExprFormatting (getGroupID groupIdVar) ++ "\n"
												numGroupsInitialisation = "call " ++ outputExprFormatting (getNumberGroups numGroupsVar) ++ "\n"
												groupSizeInitialisation = "call " ++ outputExprFormatting (getGroupSize groupSizeVar) ++ "\n"
												globalIdInitialisation = "call " ++ outputExprFormatting (getGlobalID globalIdVar) ++ "\n"

												chunk_sizeDeclaration = "integer :: " ++ outputExprFormatting chunk_size ++ "\n"

												allArgs = readVarNames ++ writtenVarNames ++ generalVarNames ++ workGroup_reductionArrays ++ global_reductionArrays ++ [chunk_size_varname]
												allArgsStr = case allArgs of
															[] -> ""
															args -> foldl (\accum item -> accum ++ "," ++ varnameStr item) (varnameStr (head args)) (tail args)
												
												
												readDecls = map (\x ->fromMaybe (NullDecl [] nullSrcSpan) (adaptOriginalDeclaration x (In []) prog)) readVarNames
												writtenDecls = map (\x ->fromMaybe (NullDecl [] nullSrcSpan) (adaptOriginalDeclaration x (Out []) prog)) writtenVarNames
												generalDecls = map (\x ->fromMaybe (NullDecl [] nullSrcSpan) (adaptOriginalDeclaration x (InOut []) prog)) generalVarNames

												readDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" (readDecls)
												writtenDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" (writtenDecls)
												generalDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" (generalDecls)
												
												local_reductionVars = map (generateLocalReductionVar) reductionVarNames
												local_reductionVarsInitStr = foldl (\accum (var, expr) -> accum ++ tabs ++ "local_" ++ varnameStr var ++ " = " ++ outputExprFormatting expr ++ "\n") "" rv


												localChunkSize_str = (outputExprFormatting localChunkSize) ++ " = " 
														++ (outputExprFormatting chunk_size) ++ " / " ++ (outputExprFormatting localSizeVar) ++ "\n"
												startPosition_str = (outputExprFormatting startPosition) ++ " = " ++ (outputExprFormatting localChunkSize) ++
													" * " ++ (outputExprFormatting globalIdVar) ++ "\n"

												reductionIterator = generateReductionIterator (r ++ w ++ (map (\(x,_,_,_) -> x) l) ++ reductionVarNames)
												workItem_loopEnd = Bin [] nullSrcSpan (Plus []) startPosition localChunkSize
												workItem_loopCode = appendFortran_recursive workItem_reductionCode workItem_loopInitialiserCode 
												workItem_loop = generateLoop reductionIterator startPosition workItem_loopEnd workItem_loopCode
												workItem_reductionCode = applyGeneratedSrcSpans (replaceAllOccurences_varnamePairs fortran reductionVarNames local_reductionVars)

												workItem_loopInitialisers = generateLoopInitialisers l (generateVar reductionIterator) Nothing
												workItem_loopInitialiserCode = foldl1 (\accum item -> appendFortran_recursive item accum) workItem_loopInitialisers

												workGroup_reductionArrays = map (generateLocalReductionArray) reductionVarNames
												-- workGroup_reductionArrays_str =	foldl (generateLocalReductionArrayArgStr) "" workGroup_reductionArrays
												workGroup_reductionArraysDecl = map (\x -> fromMaybe (NullDecl [] nullSrcSpan) (declareLocalReductionArray x (groupSizeVar) prog)) reductionVarNames
												workGroup_reductionArraysDeclStr = synthesiseDecls tabs workGroup_reductionArraysDecl
												workGroup_reductionArraysInitStr = foldl (generateReductionArrayAssignment tabs localIdVar) "" (zip workGroup_reductionArrays local_reductionVars)
												workGroup_reductionCode = generateWorkGroupReduction reductionVarNames reductionIterator fortran
												workGroup_loop = generateLoop reductionIterator (generateConstant 1) localSizeVar workGroup_reductionCode

												global_reductionArrays = map (generateGlobalReductionArray) reductionVarNames
												global_reductionArraysDecl = map (\x -> fromMaybe (NullDecl [] nullSrcSpan) (declareGlobalReductionArray x (numGroupsVar) prog)) reductionVarNames
												global_reductionArraysDeclStr = synthesiseDecls tabs global_reductionArraysDecl
												-- global_reductionArrays_str = foldl (generateGloablReductionArrayArgStr) "" global_reductionArrays
												global_reductionArraysAssignmentStr = foldl (generateReductionArrayAssignment tabs groupIdVar) "" (zip global_reductionArrays local_reductionVars)

localChunkSize = generateVar (VarName [] "local_chunk_size")
startPosition  = generateVar (VarName [] "start_position")
chunk_size = generateVar chunk_size_varname
chunk_size_varname = VarName [] "chunk_size"
localMemBarrier = "call barrier(CLK_LOCAL_MEM_FENCE)\n"

generateLocalReductionArray (VarName anno str) = VarName anno ("local_" ++ str ++ "_array")
generateGlobalReductionArray (VarName anno str) = VarName anno ("global_" ++ str ++ "_array")
generateLocalReductionArrayArgStr accum item = accum ++ "\n\t__local " ++ varnameStr item
generateGloablReductionArrayArgStr accum item = accum ++ "\n\t__global " ++ varnameStr item
generateLocalReductionVar (VarName anno str) = VarName anno ("local_" ++ str)

generateReductionArrayAssignment tabs accessor accum ((VarName _ s1),(VarName _ s2)) = accum++tabs++s1++"("++(outputExprFormatting accessor)++") = "++s2++"\n"

generateKernelName :: String -> SrcSpan -> [VarName [String]] -> String
generateKernelName identifier src varnames = identifier
											++ (foldl (\accum item -> accum ++ "_" ++ (varnameStr item)) "" varnames) 
											++ "_" ++ show (extractLineNumber src)

generateKernelCall :: Fortran [String] -> String
generateKernelCall (OpenCLMap _ src r w l fortran) = 	"\n! Global work items: " ++ outputExprFormatting workGroupSizeExpr ++ "\n "
														++ "call " ++ (generateKernelName "map" src w) 
														++ "(" ++ allArgumentsStr ++ ")"++ "\t! Call to synthesised, external kernel\n\n"
			where
				readArgs = map (varnameStr) (listSubtract r w)
				writtenArgs = map (varnameStr) (listSubtract w r)
				generalArgs = map (varnameStr) (listIntersection w r)

				allArguments = readArgs ++ writtenArgs ++ generalArgs
				allArgumentsStr =  (head allArguments) ++ foldl (\accum item -> accum ++ "," ++ item) "" (tail allArguments)

				workGroupSizeExpr = generateProductExpr (map (generateLoopIterationsExpr) l)

generateKernelCall (OpenCLReduce _ src r w l rv fortran) = 	"\n! Global work items: " ++ outputExprFormatting workGroupSizeExpr ++ "\n "
															++"call " ++ (generateKernelName "reduce" src (map (\(v, e) -> v) rv)) 
															++ "(" ++ allArgumentsStr ++ ")" ++ "\t! Call to synthesised, external kernel\n"
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

				workGroupSizeExpr = generateProductExpr (map (generateLoopIterationsExpr) l)

generateLoopIterationsExpr :: (VarName [String], Expr [String], Expr [String], Expr [String]) -> Expr [String]
generateLoopIterationsExpr (var, start, end, (Con _ _ "1")) = (Bin [] nullSrcSpan (Plus [])
																(generateSubtractionExpr [end, start]) 
																(generateConstant 1))
generateLoopIterationsExpr (var, start, end, step) = Bin [] nullSrcSpan (Div []) 
														(Bin [] nullSrcSpan (Plus [])
															(generateSubtractionExpr [end, start]) 
															(generateConstant 1))
														step

declareGlobalReductionArray :: VarName [String] -> Expr [String] -> Program [String] -> Maybe(Decl [String])
declareGlobalReductionArray varname arraySize program = case decl_list of
														[] -> Nothing
														_ -> Just (decl)
			where
				decl_list = everything (++) (mkQ [] (extractDeclaration varname)) program
				one = generateConstant 1
				newVarName = generateGlobalReductionArray varname
				decl = applyIntent (Out []) (replaceAllOccurences_varname (addDimension (head decl_list) one arraySize) varname newVarName)

declareLocalReductionArray :: VarName [String] -> Expr [String] -> Program [String] -> Maybe(Decl [String])
declareLocalReductionArray varname arraySize program = case decl_list of
														[] -> Nothing
														_ -> Just (decl)
			where
				decl_list = everything (++) (mkQ [] (extractDeclaration varname)) program
				one = generateConstant 1
				newVarName = generateLocalReductionArray varname
				-- decl = applyIntent (InOut []) (replaceAllOccurences_varname (addDimension (head decl_list) one arraySize) varname newVarName)
				decl = addDimension (applyIntent (InOut []) (replaceAllOccurences_varname (head decl_list) varname newVarName)) one arraySize

addDimension :: Decl [String] -> Expr [String] -> Expr [String] -> Decl [String]
addDimension decl start end = newDecl
			where 
				dimensions = everything (++) (mkQ [] extractDimensionAttr) decl
				newDecl = case dimensions of
							[] -> everywhere (mkT (addNewDimensionClaus start end)) decl
							_ -> everywhere (mkT (appendDimension start end)) decl

addNewDimensionClaus :: Expr [String] -> Expr [String] -> [Attr [String]] -> [Attr [String]]
-- addNewDimensionClaus start end attrList = attrList ++ [(Dimension [] [(start, end)])]
addNewDimensionClaus start end [] = [(Dimension [] [(start, end)])]
addNewDimensionClaus start end (attr:attrList) = case attr of
										Intent _ _ -> [attr] ++ attrList
										_ -> [attr] ++ addNewDimensionClaus start end attrList

appendDimension :: Expr [String] -> Expr [String] -> Attr [String] -> Attr [String]
appendDimension start end (Dimension anno lst) = Dimension anno (lst ++ [(start, end)])

extractDimensionAttr :: Attr [String] -> [Attr [String]]
extractDimensionAttr attr = case attr of
								Dimension _ _ -> [attr]
								_ -> [] 

adaptOriginalDeclaration :: VarName [String] -> IntentAttr [String] -> Program [String] -> Maybe(Decl [String])
adaptOriginalDeclaration varname intent program = case decl_list of
														[] -> Nothing
														_ -> Just (decl)
			where 
				decl_list = everything (++) (mkQ [] (extractDeclaration varname)) program
				decl = applyGeneratedSrcSpans (applyIntent (intent) (head decl_list))

applyIntent :: IntentAttr [String] -> Decl [String] -> Decl [String]
applyIntent intent decl =  newDecl
			where 
				intentAttrs = everything (++) (mkQ [] extractintentAttrs) decl
				newDecl = case intentAttrs of
							[] -> everywhere (mkT (addIntent intent)) decl
							_ -> everywhere (mkT (replaceIntent intent)) decl
				-- everywhere (mkT (replaceIntent intent)) decl

extractintentAttrs :: IntentAttr [String] -> [IntentAttr [String]]
extractintentAttrs intentAttr = [intentAttr]

replaceIntent :: IntentAttr [String] -> IntentAttr [String] -> IntentAttr [String]
replaceIntent newIntent oldIntent = newIntent

addIntent :: IntentAttr [String] -> [Attr [String]] -> [Attr [String]]
addIntent intent [] = [Intent [] intent]
addIntent intent (attr:attrList) = case attr of
										Intent _ _ -> [attr] ++ attrList
										_ -> [attr] ++ addIntent intent attrList

extractDeclaration :: VarName [String] -> Decl [String] -> [Decl [String]]
extractDeclaration varname  (Decl anno src lst typ)  	| firstHasVar || secondHasVar = [Decl anno src lst typ]
														| otherwise = []
			where
				firstExprs = map (\(expr, _, _) -> expr) lst
				secondExprs = map (\(_, expr, _) -> expr) lst

				firstHasVar = foldl (\accum item -> accum || hasVarName [varname] item) False firstExprs
				secondHasVar = foldl (\accum item -> accum || hasVarName [varname] item) False secondExprs
extractDeclaration varname decl = []

getOriginalDeclaration :: [String] -> VarName [String] -> Program [String] -> Maybe(String)
getOriginalDeclaration originalLines varname program = case declSrc_list of
														[] -> Nothing
														_ -> Just (extractOriginalCode "" originalLines declSrc)
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
extractOriginalCode :: String -> [String] -> SrcSpan -> String
extractOriginalCode tabs originalLines src = orignalFileChunk
					where 
						((SrcLoc f lineStart columnStart), (SrcLoc _ lineEnd columnEnd)) = src
						orignalFileChunk = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [lineStart..lineEnd]

generateLoop :: VarName [String] -> Expr [String] -> Expr [String] -> Fortran[String] -> Fortran[String]
generateLoop r_iter start end fortran = For [] nullSrcSpan r_iter start end step fortran
					where
						step = Con [] nullSrcSpan "1"

generateWorkGroupReduction :: [VarName [String]] -> VarName [String] -> Fortran [String] -> Fortran [String]
generateWorkGroupReduction reductionVars redIter codeSeg  = resultantCode
					where
						assignments = everything (++) (mkQ [] (generateWorkGroupReduction_assgs reductionVars redIter)) codeSeg
						resultantCode = foldl1 (\accum item -> appendFortran_recursive item accum) assignments

generateWorkGroupReduction_assgs :: [VarName [String]] -> VarName [String] -> Fortran [String] -> [Fortran [String]]
generateWorkGroupReduction_assgs reductionVars redIter (Assg _ _ expr1 expr2) 	| isReductionExpr = resultantAssg
																				| otherwise = []
					where 
						isReductionExpr = hasVarName reductionVars expr1
						resultantAssg = case extractPrimaryReductionOp expr1 expr2 of
											Just op -> [Assg [] nullSrcSpan localReductionVar (Bin [] nullSrcSpan op localReductionVar localReductionArray)]
											Nothing -> case extractPrimaryReductionFunction expr1 expr2 of
														"" -> []
														funcName -> [Assg [] nullSrcSpan localReductionVar (Var [] nullSrcSpan [(VarName [] funcName, [localReductionVar, localReductionArray])])]
						localReductionArray = generateArrayVar (generateLocalReductionArray (head (extractVarNames expr1))) (generateVar redIter)
						localReductionVar = generateVar (generateLocalReductionVar (head (extractVarNames expr1)))
generateWorkGroupReduction_assgs reductionVars redIter codeSeg = []

generateFinalHostReduction :: [VarName [String]] -> VarName [String] -> Fortran [String] -> Fortran [String]
generateFinalHostReduction reductionVars redIter codeSeg  = resultantCode
					where
						assignments = everything (++) (mkQ [] (generateFinalHostReduction_assgs reductionVars redIter)) codeSeg
						resultantCode = foldl1 (\accum item -> appendFortran_recursive item accum) assignments

generateFinalHostReduction_assgs :: [VarName [String]] -> VarName [String] -> Fortran [String] -> [Fortran [String]]
generateFinalHostReduction_assgs reductionVars redIter (Assg _ _ expr1 expr2) 	| isReductionExpr = resultantAssg
																				| otherwise = []
					where 
						isReductionExpr = hasVarName reductionVars expr1
						resultantAssg = case extractPrimaryReductionOp expr1 expr2 of
											Just op -> [Assg [] nullSrcSpan finalReductionVar (Bin [] nullSrcSpan op finalReductionVar finalReductionArray)]
											Nothing -> case extractPrimaryReductionFunction expr1 expr2 of
														"" -> []
														funcName -> [Assg [] nullSrcSpan finalReductionVar (Var [] nullSrcSpan [(VarName [] funcName, [finalReductionVar, finalReductionArray])])]
						--localReductionArray = generateArrayVar (generateLocalReductionArray (head (extractVarNames expr1))) (generateVar redIter)
						--localReductionVar = generateVar (generateLocalReductionVar (head (extractVarNames expr1)))
						finalReductionArray = generateArrayVar (generateGlobalReductionArray (head (extractVarNames expr1))) (generateVar redIter)
						finalReductionVar = generateVar (head (extractVarNames expr1))
generateFinalHostReduction_assgs reductionVars redIter codeSeg = []

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

getGlobalID :: Expr [String] ->  Expr [String]
getGlobalID globalIdVar = Var [] nullSrcSpan [(VarName [] "get_global_id", [globalIdVar, Con [] nullSrcSpan "0"])]

getGroupID :: Expr [String] ->  Expr [String]
getGroupID groupIdVar = Var [] nullSrcSpan [(VarName [] "get_group_id", [groupIdVar, Con [] nullSrcSpan "0"])]

getNumberGroups :: Expr [String] ->  Expr [String]
getNumberGroups numberGroupsVar = Var [] nullSrcSpan [(VarName [] "get_num_groups", [numberGroupsVar, Con [] nullSrcSpan "0"])]

getGroupSize :: Expr [String] ->  Expr [String]
getGroupSize groupSizeVar = Var [] nullSrcSpan [(VarName [] "get_group_size", [groupSizeVar, Con [] nullSrcSpan "0"])]

getLocalSize :: Expr [String] -> Expr [String]
getLocalSize localSizeVar = Var [] nullSrcSpan [(VarName [] "get_local_size", [localSizeVar, Con [] nullSrcSpan "0"])]

getLocalId :: Expr [String] -> Expr [String]
getLocalId localIdVar = Var [] nullSrcSpan [(VarName [] "get_local_id", [localIdVar, Con [] nullSrcSpan "0"])]

generateReductionIterator :: [VarName [String]] -> VarName [String]
generateReductionIterator usedNames = VarName [] choice
			where
				possibles = ["reduction_iterator", "reduction_iter", "r_iter"]
				choice = foldl1 (\accum item -> if not (elem (VarName [] item) usedNames) then item else accum) possibles

tabInc :: String
tabInc = "\t"