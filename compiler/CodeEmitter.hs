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

--	The strategy taken is one of traversing the AST and emitting any unchanged host code. Where kernels are encountered,
--	calls to the new kernel are emittied. The AST is then traversed again to extract all kernels and then the kernels are
--	emitted.
emit :: String -> Maybe(String) -> Program Anno -> IO ()
emit filename specified ast = do
				let newFilename = case specified of
							Nothing -> defaultFilename (splitOn "/" filename)
							Just spec -> spec
							-- "" -> defaultFilename (splitOn "/" filename)
							-- _ -> specified
				let kernelModuleNamePath = generateKernelModuleName (splitOn "/" newFilename)
				let kernelModuleName = getModuleName kernelModuleNamePath
				
				originalLines <- readOriginalFileLines filename
				let originalListing = case originalLines of
										[]	-> ""
										_ -> foldl (\accum item -> accum ++ "\n" ++ item) (head originalLines) (tail originalLines)
				let originalFileName = generateOriginalFileName (splitOn "/" newFilename)

				let code = produceCodeProg kernelModuleName originalLines ast
				let kernels = extractKernels originalLines ast
				let kernelModuleHeader = "module " ++ kernelModuleName ++ "\n\n" ++ tabInc ++ "contains\n\n"
				let kernelModuleFooter = "end module " ++ kernelModuleName

				writeFile originalFileName originalListing
				writeFile newFilename code
				writeFile kernelModuleNamePath (kernelModuleHeader ++ kernels ++ kernelModuleFooter)

--	The following functions are used to define names for output files from the input files' names.
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

--	This function produces a list of strings where each element is a line of the original source. This
--	list is used heavily in this module.
readOriginalFileLines :: String -> IO ([String])
readOriginalFileLines filename = do
				content <- cpp filename
				let contentLines = lines content
				return contentLines

extractKernels :: [String] -> Program Anno -> String
extractKernels originalLines prog  = everything (++) (mkQ "" (synthesiseKernels originalLines prog)) prog

synthesiseKernels :: [String] -> Program Anno -> Fortran Anno -> String
synthesiseKernels originalLines prog codeSeg = case codeSeg of
				OpenCLMap _ _ _ _ _ _ -> synthesiseOpenCLMap "" originalLines prog codeSeg
				OpenCLReduce _ _ _ _ _ _ _ ->  synthesiseOpenCLReduce "" originalLines prog codeSeg
				_ -> ""

produceCodeProg :: String -> [String] -> Program Anno -> String
produceCodeProg kernelModuleName originalLines prog = foldl (\accum item -> accum ++ produceCodeProgUnit kernelModuleName originalLines item) "" prog

--	This function handles producing the code that is not changed from the original source. It also inserts a "use .."
--	line for the file containing the new kernels and makes a call to the function that produces the body of the new
--	host code. 
produceCodeProgUnit :: String -> [String] -> ProgUnit Anno -> String
produceCodeProgUnit kernelModuleName originalLines progUnit = nonGeneratedHeaderCode 
												 ++ tabInc ++ "use " ++ kernelModuleName ++ "\n" 
												 ++ nonGeneratedBlockCode 
												 ++ everything (++) (mkQ "" (produceCodeBlock originalLines)) progUnit
												 ++ nonGeneratedFooterCode
												
							where

								progUnitSrc = srcSpan progUnit
								firstFortranSrc = head (everything (++) (mkQ [] (getFirstFortranSrc)) progUnit)
								firstBlockSrc = head (everything (++) (mkQ [] (getFirstBlockSrc)) progUnit)
								(nonGeneratedHeaderSrc, nonGeneratedFooterSrc) = getSrcSpanNonIntersection progUnitSrc firstBlockSrc
								-- (nonGeneratedHeaderSrc, nonGeneratedFooterSrc) = getSrcSpanNonIntersection progUnitSrc firstFortranSrc

								((SrcLoc _ nonGeneratedHeader_ls _), (SrcLoc _ nonGeneratedHeader_le _)) = nonGeneratedHeaderSrc
								nonGeneratedHeaderCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedHeader_ls..nonGeneratedHeader_le-1]

								((SrcLoc _ nonGeneratedFooter_ls _), (SrcLoc _ nonGeneratedFooter_le _)) = nonGeneratedFooterSrc
								nonGeneratedFooterCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedFooter_ls-1..nonGeneratedFooter_le-1]

								((SrcLoc _ block_ls _), (SrcLoc _ _ _)) = firstBlockSrc
								((SrcLoc _ fortran_ls _), (SrcLoc _ _ _)) = firstFortranSrc
								nonGeneratedBlockCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [block_ls+1..fortran_ls-1]

--	Function is used (along with "getFirstBlockSrc") by "produceCodeProgUnit" to determine which lines of the original
--	source can be taken as is. It is used to determine where the first Fortran nodes of the AST appear in the source
--	because the Fortran nodes are the ones that have been transformed.
getFirstFortranSrc :: Block Anno -> [SrcSpan]
getFirstFortranSrc (Block _ _ _ _ _ fortran) = [srcSpan fortran]

getFirstBlockSrc :: Block Anno -> [SrcSpan]
getFirstBlockSrc codeSeg = [srcSpan codeSeg]

produceCodeBlock :: [String] -> Block Anno -> String
produceCodeBlock originalLines block = foldl (++) "" (gmapQ (mkQ "" (produceCode_fortran "" originalLines)) block)

produceCode_fortran :: String -> [String] -> Fortran Anno -> String
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
									hostReductionLoop = generateLoop r_iter (generateConstant 1) nunitsVar hostReduction
									
						FSeq _ _ fortran1 fortran2 -> (mkQ "" (produceCode_fortran tabs originalLines) fortran1) ++ (mkQ "" (produceCode_fortran tabs originalLines) fortran2)
						_ -> 	case anyChildGenerated codeSeg || isGenerated codeSeg of
									True -> foldl (++) tabs (gmapQ (mkQ "" (produceCode_fortran "" originalLines)) codeSeg)
									False -> extractOriginalCode tabs originalLines (srcSpan codeSeg)

synthesisUses :: String -> Uses Anno -> String
synthesisUses tabs (Use _ (str, rename) _ _) = tabs ++ "use " ++ str ++ "\n"
synthesisUses tabs _ = ""

synthesiseFor :: String -> [String] -> Fortran Anno -> String
synthesiseFor tabs originalLines (For anno src varname expr1 expr2 expr3 fort) 	|	partialGenerated = tabs ++ "do " ++ (varnameStr varname) ++ "=" ++ outputExprFormatting expr1 
																								++ ", " ++ outputExprFormatting expr2 ++ (if expr3isOne then "" else outputExprFormatting expr3)
																								++ "\n" ++ (mkQ "" (produceCode_fortran (tabs ++ tabInc) originalLines) fort) ++ tabs ++ "end do\n"
																				|	otherwise = extractOriginalCode tabs originalLines src
																	where
																		expr3isOne = case expr3 of
																						Con _ _ "1" -> True
																						_ -> False
																		partialGenerated = anyChildGenerated codeSeg || isGenerated codeSeg
																		codeSeg = For anno src varname expr1 expr2 expr3 fort

synthesiseAssg :: String -> [String] -> Fortran Anno -> String
synthesiseAssg tabs originalLines (Assg anno src expr1 expr2)	|	partialGenerated = tabs ++ outputExprFormatting expr1 ++ " = " ++ outputExprFormatting expr2 ++ "\n"
															|	otherwise = extractOriginalCode tabs originalLines src
											where 
												partialGenerated = isGenerated codeSeg
												codeSeg = Assg anno src expr1 expr2

synthesiseIf :: String -> [String] -> Fortran Anno -> String
synthesiseIf tabs originalLines (If anno src expr fortran lst maybeFort) 	|	partialGenerated = tabs ++ "If (" ++ outputExprFormatting expr ++ ") then\n" 
																	++ mainFortranStr
																	++ elseIfFortranStr
																	++ elseFortranStr
																	++ tabs ++ "end if\n"
															|	otherwise = extractOriginalCode tabs originalLines src
											where 
												partialGenerated = anyChildGenerated codeSeg || isGenerated codeSeg
												codeSeg = If anno src expr fortran lst maybeFort
												mainFortranStr = (mkQ "" (produceCode_fortran (tabs ++ tabInc) originalLines) fortran)
												elseIfFortranStr = foldl (synthesisElses tabs originalLines) "" lst
												elseFortranStr = case maybeFort of
																	Just a -> (mkQ "" (produceCode_fortran (tabs ++ tabInc) originalLines) a)
																	Nothing -> ""

synthesisElses :: String -> [String] -> String -> (Expr Anno, Fortran Anno) -> String
synthesisElses tabs originalLines accum (expr, fortran) = accum ++ tabs ++ "else if(" ++ outputExprFormatting expr ++ ") then\n" 
																++ (mkQ "" (produceCode_fortran (tabs ++ tabInc) originalLines) fortran)
																++ "\n"

synthesiseDecl :: String -> Decl Anno -> String
synthesiseDecl tabs (Decl anno src lst typ) = tabs ++ (synthesiseType typ) ++ " :: " ++ (synthesiseDeclList lst) ++ "\n"
synthesiseDecl tabs (NullDecl nullAnno nullSrcSpan) = tabs ++ "[Variable not declared in orignal source]\n"
synthesiseDecl tabs _ = tabs ++ "[Unimplemented declaration syntax] \n"

synthesiseDecls :: String -> [Decl Anno] -> String
synthesiseDecls tabs decls = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" decls

synthesiseType :: Type Anno -> String
synthesiseType (BaseType anno base attrList (expr1) (expr2)) = baseStr ++ kindStr ++ attrStr
											where
												baseStr = synthesiseBaseType base
												kindStr = case outputExprFormatting expr1 of
																"" -> ""
																str -> "(kind=" ++ str ++ ")"
												attrStr = case synthesiseAttrList attrList of
																"" -> ""
																str -> ", " ++ str

synthesiseBaseType :: BaseType Anno -> String
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

synthesiseAttrList :: [Attr Anno] -> String
synthesiseAttrList [] = ""
synthesiseAttrList (attr:[]) = if paramCheck_attr attr then "" else synthesiseAttr attr
synthesiseAttrList attrList = attrStrs
				where 
					attrStrList = map (synthesiseAttr) (filter (\x -> not (paramCheck_attr x)) attrList)
					attrStrs = foldl (\accum item -> accum ++ ", " ++ item) (head attrStrList) (tail attrStrList)

synthesiseAttr :: Attr Anno -> String
synthesiseAttr (Dimension _ exprList) = "Dimension(" ++ synthesiseRangeExpr exprList ++ ")"
synthesiseAttr (Intent _ intentAttr) = "Intent(" ++ intentStr ++ ")"
					where 
						intentStr = case intentAttr of
							In _ -> "In"
							Out _ -> "Out"
							_ -> "InOut"
synthesiseAttr (Parameter _) = "Parameter"
synthesiseAttr attr = "[Incompatible attribute]"
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

synthesiseRangeExpr :: [(Expr Anno, Expr Anno)] -> String
synthesiseRangeExpr [] = ""
synthesiseRangeExpr ((NullExpr _ _, expr2):[]) = outputExprFormatting expr2
synthesiseRangeExpr ((NullExpr _ _, expr2):xs) = outputExprFormatting expr2 ++ "," ++ synthesiseRangeExpr xs
synthesiseRangeExpr ((expr1, expr2):[]) = outputExprFormatting expr1 ++ ":" ++ outputExprFormatting expr2
synthesiseRangeExpr ((expr1, expr2):xs) = outputExprFormatting expr1 ++ ":" ++ outputExprFormatting expr2 ++ "," ++ synthesiseRangeExpr xs

synthesiseDeclList :: [(Expr Anno, Expr Anno, Maybe Int)] -> String
synthesiseDeclList ((expr1, expr2, maybeInt):xs) = outputExprFormatting expr1 ++ 
												(case expr2 of
													NullExpr _ _ -> ""
													_ ->  " = " ++ outputExprFormatting expr2)
												++ maybeIntStr
												++ followingItems
						where
							maybeIntStr = case maybeInt of
											Just a -> ", " ++ show a
											Nothing -> ""
							followingItems = case xs of 
												[] -> ""
												_ -> ", " ++ synthesiseDeclList xs

synthesiseOpenCLMap :: String -> [String] -> Program Anno -> Fortran Anno -> String
synthesiseOpenCLMap inTabs originalLines prog (OpenCLMap _ src r w l fortran) = -- "\n! " ++ compilerName ++ ": Synthesised kernel\n" 
																	inTabs ++ "subroutine " ++ kernelName
																	++"(" 
																					++ allArgsStr ++ ")\n"
																	++ usesString
																	++ "\n"
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
												extractedUses = everything (++) (mkQ [] getUses) prog
												usesString = foldl (\accum item -> accum ++ synthesisUses tabs item) "" extractedUses 

												kernelName = generateKernelName "map" src w
												globalIdVar = generateVar (VarName nullAnno "global_id")
												tabs = inTabs ++ tabInc

												readArgs = listSubtract r w
												writtenArgs = listSubtract w r
												generalArgs = listIntersection w r

												allArgs = readArgs ++ writtenArgs ++ generalArgs
												allArgsStr = case allArgs of
															[] -> ""
															args -> foldl (\accum item -> accum ++ "," ++ varnameStr item) (varnameStr (head args)) (tail args)
												
												
												readDecls = map (\x ->fromMaybe (NullDecl nullAnno nullSrcSpan) (adaptOriginalDeclaration_intent x (In nullAnno) prog)) readArgs
												writtenDecls = map (\x ->fromMaybe (NullDecl nullAnno nullSrcSpan) (adaptOriginalDeclaration_intent x (Out nullAnno) prog)) writtenArgs
												generalDecls = map (\x ->fromMaybe (NullDecl nullAnno nullSrcSpan) (adaptOriginalDeclaration_intent x (InOut nullAnno) prog)) generalArgs

												readDeclStr = foldl (\accum item -> accum ++ synthesiseDecl (tabs) item) "" (readDecls)
												writtenDeclStr = foldl (\accum item -> accum ++ synthesiseDecl (tabs) item) "" (writtenDecls)
												generalDeclStr = foldl (\accum item -> accum ++ synthesiseDecl (tabs) item) "" (generalDecls)

												loopInitialisers = generateLoopInitialisers l globalIdVar Nothing
												loopInitialiserCode = case loopInitialisers of
																		[] -> error "synthesiseOpenCLMap: loopInitialiserCode - empty list"
																		_ -> foldl1 (\accum item -> appendFortran_recursive item accum) loopInitialisers


synthesiseOpenCLReduce :: String ->  [String] -> Program Anno -> Fortran Anno -> String
synthesiseOpenCLReduce inTabs originalLines prog (OpenCLReduce _ src r w l rv fortran)  = -- "\n! " ++ compilerName ++ ": Synthesised kernel\n" 
																			inTabs ++ "subroutine " ++ kernelName
																			++ "(" 				++ allArgsStr
																								-- ++ workGroup_reductionArrays_str 
																								-- ++ "workGroup_reductionArrays_str"
																								-- ++ global_reductionArrays_str 
																								-- ++ chunkSize_str 
																								++ ")\n"
																			++ usesString
																			++ "\n"
																			++ tabs ++ chunk_sizeDeclaration
																			++ tabs ++ localSizeDeclaration 
																			++ tabs ++ localIdDeclaration
																			++ tabs ++ groupIdDeclaration
																			++ tabs ++ numGroupsDeclaration
																			-- ++ tabs ++ groupSizeDeclaration ++ " = " ++ outputExprFormatting groupSizeInitialisation_calculation ++ "\n"
																			++ tabs ++ globalIdDeclaration 
																			++ tabs ++ reductionIteratorDeclaration
																			++ tabs ++ localChunkSizeDeclaration
																			++ tabs ++ startPositionDeclaration
																			++ "\n"
																			++ readDeclStr
																			++ writtenDeclStr
																			++ generalDeclStr
																			++ "\n"
																			++ workGroup_reductionArraysDeclStr
																			++ global_reductionArraysDeclStr
																			++ local_reductionVarsDeclatationStr
																			++ "\n"
																			++ tabs ++ localSizeInitialisation
																			++ tabs ++ localIdInitialisation
																			++ tabs ++ groupIdInitialisation
																			++ tabs ++ numGroupsInitialisation
																			-- ++ tabs ++ groupSizeInitialisation
																			++ tabs ++ globalIdInitialisation
																			-- ++ "! " ++ compilerName ++ ": Reduction vars: " ++ global_reductionVars ++ "\n"
																			++ tabs ++ localChunkSize_str
																			++ tabs ++ startPosition_str
																			-- ++ "! " ++ compilerName ++ ": Synthesised loop variables\n"
																			-- ++ produceCode_fortran originalLines local_loopInitialiserCode ++ "\n\n"
																			-- ++ tabs ++ "! " ++ compilerName ++ ": Work item reduction\n" 
																			++ local_reductionVarsInitStr
																			++ "\n"
																			++ (mkQ "" (produceCode_fortran (tabs) originalLines) workItem_loop)
																			++ "\n"
																			++ workGroup_reductionArraysInitStr
																			++ "\n"
																			++ tabs ++ localMemBarrier
																			++ "\n"
																			++ local_reductionVarsInitStr
																			-- ++ "\n! Workgroup reduction\n"
																			++ (mkQ "" (produceCode_fortran (tabs) originalLines) workGroup_loop)
																			-- ++ "\n! Write to output array(s) goes here\n"
																			++ global_reductionArraysAssignmentStr
																			++ "\n"
																			++ inTabs ++ "end subroutine " ++ kernelName
																			++"\n\n\n"
																			-- ++ "! " ++ compilerName ++ ": End of synthesised kernel\n\n" 
											where
												kernelName = generateKernelName "reduce" src (map (\(v, e) -> v) rv)
												tabs = inTabs ++ tabInc

												extractedUses = everything (++) (mkQ [] getUses) prog
												usesString = foldl (\accum item -> accum ++ synthesisUses tabs item) "" extractedUses 

												reductionVarNames = map (\(varname, expr) -> varname) rv
												readVarNames = listSubtract (listSubtract r w) reductionVarNames
												writtenVarNames = listSubtract (listSubtract w r) reductionVarNames
												generalVarNames = listSubtract (listIntersection w r) reductionVarNames

												localSizeVar = generateVar (VarName nullAnno "local_size")
												localIdVar = generateVar (VarName nullAnno "local_id")
												groupIdVar = generateVar (VarName nullAnno "group_id")
												--numGroupsVar = generateVar (VarName nullAnno "num_groups")
												--groupSizeVar = generateVar (VarName nullAnno "group_size")
												globalIdVar = generateVar (VarName nullAnno "global_id")

												localSizeDeclaration = "integer :: " ++ outputExprFormatting localSizeVar ++ "\n"
												localIdDeclaration = "integer :: " ++ outputExprFormatting localIdVar ++ "\n"
												groupIdDeclaration = "integer :: " ++ outputExprFormatting groupIdVar ++ "\n"
												numGroupsDeclaration = "integer :: " ++ outputExprFormatting numGroupsVar ++ "\n"
												--groupSizeDeclaration = "integer, Parameter :: " ++ outputExprFormatting groupSizeVar
												globalIdDeclaration = "integer :: " ++ outputExprFormatting globalIdVar ++ "\n"
												reductionIteratorDeclaration = "integer :: " ++ varnameStr reductionIterator ++ "\n"
												localChunkSizeDeclaration = "integer :: " ++ outputExprFormatting localChunkSize ++ "\n"
												startPositionDeclaration = "integer :: " ++ outputExprFormatting startPosition ++ "\n"
												chunk_sizeDeclaration = "integer :: " ++ outputExprFormatting chunk_size ++ "\n"

												localSizeInitialisation = "call " ++ outputExprFormatting (getLocalSize localSizeVar) ++ "\n"
												localIdInitialisation = "call " ++ outputExprFormatting (getLocalId localIdVar) ++ "\n"
												groupIdInitialisation = "call " ++ outputExprFormatting (getGroupID groupIdVar) ++ "\n"
												numGroupsInitialisation = "call " ++ outputExprFormatting (getNumberGroups numGroupsVar) ++ "\n"
												--groupSizeInitialisation = "call " ++ outputExprFormatting (getGroupSize groupSizeVar) ++ "\n"
												globalIdInitialisation = "call " ++ outputExprFormatting (getGlobalID globalIdVar) ++ "\n"
												groupSizeInitialisation_calculation = generateGlobalWorkItemsExpr l

												--allArgs = readVarNames ++ writtenVarNames ++ generalVarNames ++ workGroup_reductionArrays ++ global_reductionArrays ++ [chunk_size_varname]
												allArgs = readVarNames ++ writtenVarNames ++ generalVarNames ++ global_reductionArrays
												allArgsStr = case allArgs of
															[] -> ""
															args -> foldl (\accum item -> accum ++ "," ++ varnameStr item) (varnameStr (head args)) (tail args)
												
												
												readDecls = removeDeclAssignments $ map (\x ->fromMaybe (NullDecl nullAnno nullSrcSpan) (adaptOriginalDeclaration_intent x (In nullAnno) prog)) readVarNames
												writtenDecls = removeDeclAssignments $ map (\x ->fromMaybe (NullDecl nullAnno nullSrcSpan) (adaptOriginalDeclaration_intent x (Out nullAnno) prog)) writtenVarNames
												generalDecls = removeDeclAssignments $ map (\x ->fromMaybe (NullDecl nullAnno nullSrcSpan) (adaptOriginalDeclaration_intent x (InOut nullAnno) prog)) generalVarNames

												readDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" (readDecls)
												writtenDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" (writtenDecls)
												generalDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" (generalDecls)
												
												local_reductionVars = map (generateLocalReductionVar) reductionVarNames
												local_reductionVarsInitStr = "! local_reductionVarsInitStr\n" ++ foldl (\accum (var, expr) -> accum ++ tabs ++ "local_" ++ varnameStr var ++ " = " ++ outputExprFormatting expr ++ "\n") "" rv
												local_reductionVarsDeclatation = map (\(red, local) -> stripDeclAttrs $ fromMaybe (NullDecl nullAnno nullSrcSpan) (adaptOriginalDeclaration_varname red local prog)) (zip reductionVarNames local_reductionVars)
												local_reductionVarsDeclatationStr = synthesiseDecls tabs local_reductionVarsDeclatation

												--localChunkSize_str = (outputExprFormatting localChunkSize) ++ " = " 
												--		++ (outputExprFormatting chunk_size) ++ " / " ++ (outputExprFormatting localSizeVar) ++ "\n"
												localChunkSize_assg = generateAssgCode 
																			localChunkSize 
																			(generateDivisionExpr
																				(generateDivisionExpr 
																					(generateGlobalWorkItemsExpr l)
																					nthVar)
																				numGroupsVar)
												localChunkSize_str = synthesiseAssg inTabs originalLines localChunkSize_assg

												startPosition_str = (outputExprFormatting startPosition) ++ " = " ++ (outputExprFormatting localChunkSize) ++
													" * " ++ (outputExprFormatting globalIdVar) ++ "\n"

												reductionIterator = generateReductionIterator (r ++ w ++ (map (\(x,_,_,_) -> x) l) ++ reductionVarNames)
												workItem_loopEnd = Bin nullAnno nullSrcSpan (Plus nullAnno) startPosition localChunkSize
												workItem_loopCode = appendFortran_recursive workItem_reductionCode workItem_loopInitialiserCode 
												workItem_loop = generateLoop reductionIterator startPosition workItem_loopEnd workItem_loopCode
												workItem_reductionCode = applyGeneratedSrcSpans (replaceAllOccurences_varnamePairs fortran reductionVarNames local_reductionVars)

												workItem_loopInitialisers = generateLoopInitialisers l (generateVar reductionIterator) Nothing
												workItem_loopInitialiserCode = case workItem_loopInitialisers of
																				[] -> error "synthesiseOpenCLReduce: workItem_loopInitialiserCode - empty list"
																				_ -> foldl1 (\accum item -> appendFortran_recursive item accum) workItem_loopInitialisers

												workGroup_reductionArrays = map (generateLocalReductionArray) reductionVarNames
												-- workGroup_reductionArrays_str =	foldl (generateLocalReductionArrayArgStr) "" workGroup_reductionArrays
												workGroup_reductionArraysDecl = map (\x -> fromMaybe (NullDecl nullAnno nullSrcSpan) (declareLocalReductionArray x (nthVar) prog)) reductionVarNames
												workGroup_reductionArraysDeclStr = synthesiseDecls tabs workGroup_reductionArraysDecl
												workGroup_reductionArraysInitStr = foldl (generateReductionArrayAssignment tabs localIdVar) "" (zip workGroup_reductionArrays local_reductionVars)
												workGroup_reductionCode = generateWorkGroupReduction reductionVarNames reductionIterator fortran
												workGroup_loop = generateLoop reductionIterator (generateConstant 1) localSizeVar workGroup_reductionCode

												global_reductionArrays = map (generateGlobalReductionArray) reductionVarNames
												global_reductionArraysDecl = map (\x -> fromMaybe (NullDecl nullAnno nullSrcSpan) (declareGlobalReductionArray x (nunitsVar) prog)) reductionVarNames
												global_reductionArraysDeclStr = synthesiseDecls tabs global_reductionArraysDecl
												-- global_reductionArrays_str = foldl (generateGloablReductionArrayArgStr) "" global_reductionArrays
												global_reductionArraysAssignmentStr = foldl (generateReductionArrayAssignment tabs groupIdVar) "" (zip global_reductionArrays local_reductionVars)

localChunkSize = generateVar (VarName nullAnno "local_chunk_size")
startPosition  = generateVar (VarName nullAnno "start_position")
chunk_size = generateVar chunk_size_varname
chunk_size_varname = VarName nullAnno "chunk_size"
localMemBarrier = "call barrier(CLK_LOCAL_MEM_FENCE)\n"
nthVar = generateVar (VarName nullAnno "NTH")
nunitsVar = generateVar (VarName nullAnno "NUNITS")
numGroupsVarName = VarName nullAnno "num_groups"
numGroupsVar = generateVar numGroupsVarName


generateLocalReductionArray (VarName anno str) = VarName anno ("local_" ++ str ++ "_array")
generateGlobalReductionArray (VarName anno str) = VarName anno ("global_" ++ str ++ "_array")
generateLocalReductionArrayArgStr accum item = accum ++ "\n" ++ tabInc ++ "__local " ++ varnameStr item
generateGloablReductionArrayArgStr accum item = accum ++ "\n" ++ tabInc ++ "__global " ++ varnameStr item
generateLocalReductionVar (VarName anno str) = VarName anno ("local_" ++ str)

generateReductionArrayAssignment tabs accessor accum ((VarName _ s1),(VarName _ s2)) = accum++tabs++s1++"("++(outputExprFormatting accessor)++") = "++s2++"\n"

generateKernelName :: String -> SrcSpan -> [VarName Anno] -> String
generateKernelName identifier src varnames = identifier
											++ (foldl (\accum item -> accum ++ "_" ++ (varnameStr item)) "" varnames) 
											++ "_" ++ show (extractLineNumber src)

generateKernelCall :: Fortran Anno -> String
generateKernelCall (OpenCLMap _ src r w l fortran) = 	"! Global work items: " ++ outputExprFormatting globalWorkItems ++ "\n "
														++ "call " ++ (generateKernelName "map" src w) 
														++ "(" ++ allArgumentsStr ++ ")"++ "" ++ tabInc ++ "! Call to synthesised, external kernel\n\n"
			where
				readArgs = map (varnameStr) (listSubtract r w)
				writtenArgs = map (varnameStr) (listSubtract w r)
				generalArgs = map (varnameStr) (listIntersection w r)

				allArguments = readArgs ++ writtenArgs ++ generalArgs
				allArgumentsStr =  (head allArguments) ++ foldl (\accum item -> accum ++ "," ++ item) "" (tail allArguments)

				--workGroupSizeExpr = generateProductExpr_list (map (generateLoopIterationsExpr) l)
				globalWorkItems = generateGlobalWorkItemsExpr l

generateKernelCall (OpenCLReduce _ src r w l rv fortran) = 	"\n! Global work items: " ++ outputExprFormatting reductionWorkItemsExpr ++ "\n"
															++ "! " ++ (outputExprFormatting nunitsVar) ++" = clGetDeviceInfo( CL_DEVICE_MAX_COMPUTE_UNITS )\n"
															++"call " ++ (generateKernelName "reduce" src (map (\(v, e) -> v) rv)) 
															++ "(" ++ allArgumentsStr ++ ")" ++ "" ++ tabInc ++ "! Call to synthesised, external kernel\n"
			where 
				reductionVarNames = map (\(varname, expr) -> varname) rv
				workGroup_reductionArrays = map (generateLocalReductionArray) reductionVarNames
				global_reductionArrays = map (generateGlobalReductionArray) reductionVarNames
				readArgs = listSubtract r w
				writtenArgs = listSubtract w r
				generalArgs = listIntersection w r

				allArguments = 	(map (varnameStr) 
									(listSubtract 
										(readArgs ++ writtenArgs ++ generalArgs ++ global_reductionArrays) 
										reductionVarNames)) 
				allArgumentsStr =  (head allArguments) ++ foldl (\accum item -> accum ++ "," ++ item) "" (tail allArguments)

				reductionWorkItemsExpr = generateProductExpr nthVar nunitsVar

generateLoopIterationsExpr :: (VarName Anno, Expr Anno, Expr Anno, Expr Anno) -> Expr Anno
generateLoopIterationsExpr (var, (Con _ _ "1"), end, (Con _ _ "1")) = end
generateLoopIterationsExpr (var, start, end, (Con _ _ "1")) = (Bin nullAnno nullSrcSpan (Plus nullAnno)
																(generateSubtractionExpr end start) 
																(generateConstant 1))
generateLoopIterationsExpr (var, (Con _ _ "1"), end, step) = Bin nullAnno nullSrcSpan (Div nullAnno) 
																end
																step
generateLoopIterationsExpr (var, start, end, step) = Bin nullAnno nullSrcSpan (Div nullAnno) 
														(Bin nullAnno nullSrcSpan (Plus nullAnno)
															(generateSubtractionExpr_list [end, start]) 
															(generateConstant 1))
														step

declareGlobalReductionArray :: VarName Anno -> Expr Anno -> Program Anno -> Maybe(Decl Anno)
declareGlobalReductionArray varname arraySize program = case decl_list of
														[] -> Nothing
														_ -> Just (decl)
			where
				decl_list = everything (++) (mkQ [] (extractDeclaration varname)) program
				one = generateConstant 1
				newVarName = generateGlobalReductionArray varname
				decl = applyIntent (Out nullAnno) (replaceAllOccurences_varname (addDimension (head decl_list) one arraySize) varname newVarName)

declareLocalReductionArray :: VarName Anno -> Expr Anno -> Program Anno -> Maybe(Decl Anno)
declareLocalReductionArray varname arraySize program = case decl_list of
														[] -> Nothing
														_ -> Just (decl)
			where
				decl_list = everything (++) (mkQ [] (extractDeclaration varname)) program
				one = generateConstant 1
				newVarName = generateLocalReductionArray varname
				decl = addDimension (replaceAllOccurences_varname (head decl_list) varname newVarName) one arraySize


addDimension :: Decl Anno -> Expr Anno -> Expr Anno -> Decl Anno
addDimension decl start end = newDecl
			where 
				dimensions = everything (++) (mkQ [] extractDimensionAttr) decl
				newDecl = case dimensions of
							[] -> everywhere (mkT (addNewDimensionClaus start end)) decl
							_ -> everywhere (mkT (appendDimension start end)) decl

addNewDimensionClaus :: Expr Anno -> Expr Anno -> [Attr Anno] -> [Attr Anno]
addNewDimensionClaus start end [] = [(Dimension nullAnno [(start, end)])]
addNewDimensionClaus start end (attr:attrList) = case attr of
										Intent _ _ -> [attr] ++ attrList
										_ -> [attr] ++ addNewDimensionClaus start end attrList

appendDimension :: Expr Anno -> Expr Anno -> Attr Anno -> Attr Anno
appendDimension start end (Dimension anno lst) = Dimension anno (lst ++ [(start, end)])

extractDimensionAttr :: Attr Anno -> [Attr Anno]
extractDimensionAttr attr = case attr of
								Dimension _ _ -> [attr]
								_ -> [] 

containsParameterAttr :: Decl Anno -> Bool
containsParameterAttr decl = foldl (||) False (gmapQ (mkQ False paramCheck_type) decl)

paramCheck_type :: Type Anno -> Bool
paramCheck_type (BaseType _ baseT attrList _ _) = foldl (\accum item -> accum || paramCheck_attr item) False attrList
paramCheck_type (ArrayT _ _ baseT attrList _ _) = foldl (\accum item -> accum || paramCheck_attr item) False attrList

paramCheck_attr :: Attr Anno -> Bool
paramCheck_attr (Parameter _) = True 
paramCheck_attr _ = False

adaptOriginalDeclaration_intent :: VarName Anno -> IntentAttr Anno -> Program Anno -> Maybe(Decl Anno)
adaptOriginalDeclaration_intent varname intent program = case decl_list of
														[] -> Nothing
														_ -> Just (decl)
			where 
				decl_list = everything (++) (mkQ [] (extractDeclaration varname)) program
				decl = case containsParameterAttr (head decl_list) of
							True -> applyGeneratedSrcSpans (head decl_list)
							False -> applyGeneratedSrcSpans (applyIntent (intent) (head decl_list))

adaptOriginalDeclaration_varname :: VarName Anno -> VarName Anno -> Program Anno -> Maybe(Decl Anno)
adaptOriginalDeclaration_varname varname newVarname program = case decl_list of
														[] -> Nothing
														_ -> Just (decl)
			where 
				decl_list = everything (++) (mkQ [] (extractDeclaration varname)) program
				decl = applyGeneratedSrcSpans (replaceAllOccurences_varname (head decl_list) varname newVarname)

stripDeclAttrs :: Decl Anno -> Decl Anno
stripDeclAttrs decl = everywhere (mkT stripAttrs) decl

stripAttrs :: [Attr Anno] -> [Attr Anno]
stripAttrs a = []

applyIntent :: IntentAttr Anno -> Decl Anno -> Decl Anno
applyIntent intent decl =  newDecl
			where 
				intentAttrs = everything (++) (mkQ [] extractintentAttrs) decl
				newDecl = case intentAttrs of
							[] -> everywhere (mkT (addIntent intent)) decl
							_ -> everywhere (mkT (replaceIntent intent)) decl
				-- everywhere (mkT (replaceIntent intent)) decl

extractintentAttrs :: IntentAttr Anno -> [IntentAttr Anno]
extractintentAttrs intentAttr = [intentAttr]

replaceIntent :: IntentAttr Anno -> IntentAttr Anno -> IntentAttr Anno
replaceIntent newIntent oldIntent = newIntent

addIntent :: IntentAttr Anno -> [Attr Anno] -> [Attr Anno]
addIntent intent [] = [Intent nullAnno intent]
addIntent intent (attr:attrList) = case attr of
										Intent _ _ -> [attr] ++ attrList
										_ -> [attr] ++ addIntent intent attrList

removeDeclAssignments :: [Decl Anno] -> [Decl Anno]
removeDeclAssignments decls = map (removeDeclAssignment) decls

removeDeclAssignment :: Decl Anno -> Decl Anno
removeDeclAssignment (Decl anno src assgList typ) = Decl anno src newAssgList typ
				where
					newAssgList = map (\(expr1, _, _) -> (expr1, (NullExpr nullAnno nullSrcSpan), Nothing)) assgList
removeDeclAssignment decl = decl

extractDeclaration :: VarName Anno -> Decl Anno -> [Decl Anno]
extractDeclaration varname  (Decl anno src lst typ)  	| firstHasVar || secondHasVar = [Decl anno src lst typ]
														| otherwise = []
			where
				firstExprs = map (\(expr, _, _) -> expr) lst
				secondExprs = map (\(_, expr, _) -> expr) lst

				firstHasVar = foldl (\accum item -> accum || usesVarName_list [varname] item) False firstExprs
				secondHasVar = foldl (\accum item -> accum || usesVarName_list [varname] item) False secondExprs
extractDeclaration varname decl = []

getOriginalDeclaration :: [String] -> VarName Anno -> Program Anno -> Maybe(String)
getOriginalDeclaration originalLines varname program = case declSrc_list of
														[] -> Nothing
														_ -> Just (extractOriginalCode "" originalLines declSrc)
			where 
				declSrc_list = everything (++) (mkQ [] (extractDeclarationSrcSpan varname)) program
				declSrc = head declSrc_list


extractDeclarationSrcSpan :: VarName Anno -> Decl Anno -> [SrcSpan]
extractDeclarationSrcSpan varname (Decl _ src lst _) 	| firstHasVar || secondHasVar = [src]
														| otherwise = []
			where
				firstExprs = map (\(expr, _, _) -> expr) lst
				secondExprs = map (\(_, expr, _) -> expr) lst

				firstHasVar = foldl (\accum item -> accum || usesVarName_list [varname] item) False firstExprs
				secondHasVar = foldl (\accum item -> accum || usesVarName_list [varname] item) False secondExprs
extractDeclarationSrcSpan varname decl = []


anyChildGenerated :: Fortran Anno -> Bool
anyChildGenerated ast = everything (||) (mkQ False isGenerated) ast

isGenerated :: Fortran Anno -> Bool
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

generateLoop :: VarName Anno -> Expr Anno -> Expr Anno -> Fortran Anno -> Fortran Anno
generateLoop r_iter start end fortran = For nullAnno nullSrcSpan r_iter start end step fortran
					where
						step = Con nullAnno nullSrcSpan "1"

generateWorkGroupReduction :: [VarName Anno] -> VarName Anno -> Fortran Anno -> Fortran Anno
generateWorkGroupReduction reductionVars redIter codeSeg  = resultantCode
					where
						assignments = everything (++) (mkQ [] (generateWorkGroupReduction_assgs reductionVars redIter)) codeSeg
						--resultantCode = case assignments of
						--			[] -> error "generateWorkGroupReduction: resultantCode - empty list"
						--			_ -> foldl1 (\accum item -> appendFortran_recursive item accum) assignments
						resultantCode = foldl1 (\accum item -> appendFortran_recursive item accum) assignments

generateWorkGroupReduction_assgs :: [VarName Anno] -> VarName Anno -> Fortran Anno -> [Fortran Anno]
generateWorkGroupReduction_assgs reductionVars redIter (Assg _ _ expr1 expr2) 	| isReductionExpr = resultantAssg
																				| otherwise = []
					where 
						isReductionExpr = usesVarName_list reductionVars expr1
						resultantAssg = case extractPrimaryReductionOp expr1 expr2 of
											Just op -> [Assg nullAnno nullSrcSpan localReductionVar (Bin nullAnno nullSrcSpan op localReductionVar localReductionArray)]
											Nothing -> case extractPrimaryReductionFunction expr1 expr2 of
														"" -> []
														funcName -> [Assg nullAnno nullSrcSpan localReductionVar (Var nullAnno nullSrcSpan [(VarName nullAnno funcName, [localReductionVar, localReductionArray])])]
						localReductionArray = generateArrayVar (generateLocalReductionArray (head (extractVarNames expr1))) (generateVar redIter)
						localReductionVar = generateVar (generateLocalReductionVar (head (extractVarNames expr1)))
generateWorkGroupReduction_assgs reductionVars redIter codeSeg = []

generateFinalHostReduction :: [VarName Anno] -> VarName Anno -> Fortran Anno -> Fortran Anno
generateFinalHostReduction reductionVars redIter codeSeg  = resultantCode
					where
						assignments = everything (++) (mkQ [] (generateFinalHostReduction_assgs reductionVars redIter)) codeSeg
						--resultantCode = case assignments of
						--			[] -> error "generateFinalHostReduction: resultantCode - empty list"
						--			_ -> foldl1 (\accum item -> appendFortran_recursive item accum) assignments
						resultantCode = foldl1 (\accum item -> appendFortran_recursive item accum) assignments

generateFinalHostReduction_assgs :: [VarName Anno] -> VarName Anno -> Fortran Anno -> [Fortran Anno]
generateFinalHostReduction_assgs reductionVars redIter (Assg _ _ expr1 expr2) 	| isReductionExpr = resultantAssg
																				| otherwise = resultantAssg -- []
					where 
						isReductionExpr = usesVarName_list reductionVars expr1
						resultantAssg = case extractPrimaryReductionOp expr1 expr2 of
											Just op -> [Assg nullAnno nullSrcSpan finalReductionVar (Bin nullAnno nullSrcSpan op finalReductionVar finalReductionArray)]
											Nothing -> case extractPrimaryReductionFunction expr1 expr2 of
														"" -> []
														funcName -> [Assg nullAnno nullSrcSpan finalReductionVar (Var nullAnno nullSrcSpan [(VarName nullAnno funcName, [finalReductionVar, finalReductionArray])])]
						--localReductionArray = generateArrayVar (generateLocalReductionArray (head (extractVarNames expr1))) (generateVar redIter)
						--localReductionVar = generateVar (generateLocalReductionVar (head (extractVarNames expr1)))
						finalReductionArray = generateArrayVar (generateGlobalReductionArray (head (extractVarNames expr1))) (generateVar redIter)
						finalReductionVar = generateVar (head (extractVarNames expr1))
generateFinalHostReduction_assgs reductionVars redIter codeSeg = []

generateGlobalWorkItemsExpr :: [(VarName Anno, Expr Anno, Expr Anno, Expr Anno)] -> Expr Anno
generateGlobalWorkItemsExpr loopVars = generateProductExpr_list (map (generateLoopIterationsExpr) loopVars)

generateRelVar :: VarName Anno -> Expr Anno
generateRelVar (VarName anno str) = generateVar (VarName anno (str ++ "_rel"))

generateRangeVar :: VarName Anno -> Expr Anno
generateRangeVar (VarName anno str) = generateVar (VarName anno (str ++ "_range"))

generateLoopStartAddition :: VarName Anno -> Expr Anno -> Fortran Anno
generateLoopStartAddition varname start = generateAssgCode (generateVar varname) (generateAdditionExpr (generateRelVar varname) start)

generateRangeExpr :: VarName Anno -> Expr Anno -> Expr Anno -> Fortran Anno
generateRangeExpr varname start end = generateAssgCode (generateRangeVar varname) (generateSubtractionExpr end start)

generateLoopInitialisers :: [(VarName Anno, Expr Anno, Expr Anno, Expr Anno)] -> Expr Anno -> Maybe(Expr Anno) -> [Fortran Anno]
generateLoopInitialisers ((var, start, end, step):[]) iterator Nothing 
			= 	[Assg nullAnno nullSrcSpan 
				(generateRelVar var)
				--(generateVar var)
				iterator,
				generateLoopStartAddition var start] 
generateLoopInitialisers ((var, start, end, step):[]) iterator (Just offset) 
			= 	[generateRangeExpr var start end,
				Assg nullAnno nullSrcSpan 
				(generateRelVar var)
				--(generateVar var)
				(offset),
				generateLoopStartAddition var start]

generateLoopInitialisers ((var, start, end, step):xs) iterator Nothing 
			= 	[generateRangeExpr var start end,
				Assg nullAnno nullSrcSpan 
				(generateRelVar var)
				--generateVar var)
				(Bin nullAnno nullSrcSpan (Div nullAnno)  iterator multipliedExprs),
				generateLoopStartAddition var start]
				++
				generateLoopInitialisers xs iterator (Just nextOffset)
					where
						--nextOffset = generateSubtractionExpr_list ([generateProductExpr_list ([generateVar var] ++ followingEndExprs)])
						nextOffset = generateSubtractionExpr_list ([iterator] ++ [generateProductExpr_list ([generateRelVar var] ++ followingRangeExprs)])
						--followingEndExprs = map (\(_,_,e,_) -> e) xs
						--multipliedExprs = generateProductExpr_list followingEndExprs 
						followingRangeExprs = map (\(v,_,_,_) -> generateRangeVar v) xs
						multipliedExprs = generateProductExpr_list followingRangeExprs 
generateLoopInitialisers ((var, start, end, step):xs) iterator (Just offset) 
			= 	[generateRangeExpr var start end,
				Assg nullAnno nullSrcSpan (generateRelVar var)-- (generateVar var)
					(Bin nullAnno nullSrcSpan (Div nullAnno) 
						offset
						multipliedExprs),
				generateLoopStartAddition var start]
				++
				generateLoopInitialisers xs iterator (Just nextOffset)
					where
						--nextOffset = generateSubtractionExpr_list ([offset] ++ [generateProductExpr_list ([generateVar var] ++ followingEndExprs)])
						nextOffset = generateSubtractionExpr_list ([offset] ++ [generateProductExpr_list ([generateRelVar var] ++ followingRangeExprs)])
						--followingEndExprs = map (\(_,_,e,_) -> e) xs
						--multipliedExprs = generateProductExpr_list followingEndExprs 
						followingRangeExprs = map (\(v,_,_,_) -> generateRangeVar v) xs
						multipliedExprs = generateProductExpr_list followingRangeExprs 

generateProductExpr_list :: [Expr Anno] -> Expr Anno
generateProductExpr_list (x:[]) = x
generateProductExpr_list (x:xs) = Bin nullAnno nullSrcSpan (Mul nullAnno) x (generateProductExpr_list xs)

generateSubtractionExpr_list :: [Expr Anno] -> Expr Anno
generateSubtractionExpr_list (x:[]) = x
generateSubtractionExpr_list (x:xs) = Bin nullAnno nullSrcSpan (Minus nullAnno) x (generateSubtractionExpr_list xs)

getGlobalID :: Expr Anno ->  Expr Anno
getGlobalID globalIdVar = Var nullAnno nullSrcSpan [(VarName nullAnno "get_global_id", [globalIdVar, Con nullAnno nullSrcSpan "0"])]

getGroupID :: Expr Anno ->  Expr Anno
getGroupID groupIdVar = Var nullAnno nullSrcSpan [(VarName nullAnno "get_group_id", [groupIdVar, Con nullAnno nullSrcSpan "0"])]

getNumberGroups :: Expr Anno ->  Expr Anno
getNumberGroups numberGroupsVar = Var nullAnno nullSrcSpan [(VarName nullAnno "get_num_groups", [numberGroupsVar, Con nullAnno nullSrcSpan "0"])]

getGroupSize :: Expr Anno ->  Expr Anno
getGroupSize groupSizeVar = Var nullAnno nullSrcSpan [(VarName nullAnno "get_group_size", [groupSizeVar, Con nullAnno nullSrcSpan "0"])]

getLocalSize :: Expr Anno -> Expr Anno
getLocalSize localSizeVar = Var nullAnno nullSrcSpan [(VarName nullAnno "get_local_size", [localSizeVar, Con nullAnno nullSrcSpan "0"])]

getLocalId :: Expr Anno -> Expr Anno
getLocalId localIdVar = Var nullAnno nullSrcSpan [(VarName nullAnno "get_local_id", [localIdVar, Con nullAnno nullSrcSpan "0"])]

generateReductionIterator :: [VarName Anno] -> VarName Anno
generateReductionIterator usedNames = VarName nullAnno choice
			where
				possibles = ["reduction_iterator", "reduction_iter", "r_iter"]
				choice = case possibles of
						[] -> error "generateReductionIterator: choice - empty list"
						_ -> foldl1 (\accum item -> if not (elem (VarName nullAnno item) usedNames) then item else accum) possibles

tabInc :: String
tabInc = "    "