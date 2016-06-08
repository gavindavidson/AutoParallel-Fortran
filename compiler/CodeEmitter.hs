module CodeEmitter where

--	Code in this file handles the final emission of code. The function 'emit' is called against an AST that has been transformed and has had kernels fused.
--	Trys to make as much use as it can of code that is the same as it was in the original source file. Otherwise, it uses mostly simple functions to generate
--	code segments. Things get more complex when generating reduction kernels as a number of changes to the original source are needed to take full advantage
--	of parallel hardware.

import Control.Monad
-- import qualified Data.Foldable as Mfold
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
import qualified Data.Map as DMap 

import LanguageFortranTools

emit_beta :: String -> [String] -> [(Program Anno, String)] -> IO [()]
emit_beta specified cppDFlags programs = do
				kernels_code <- mapM (extractKernels_beta cppDFlags) programs
				let allKernels = foldl (++) [] kernels_code
				let kernelNames = map (snd) allKernels

				let originalFilenames = map (\x -> getModuleName (snd x)) programs
				let moduleName = (foldl1 (\accum item -> accum ++ "_" ++ item) originalFilenames) ++ "_superkernel"
				let moduleFileName = specified ++ "/" ++ "module_" ++ moduleName ++ ".f95"
				let superKernel_module = synthesiseSuperKernelModule moduleName programs allKernels

				host_code <- mapM (produceCodeProg cppDFlags moduleName) programs
				let host_programs = zip host_code (map (\x -> specified ++ "/" ++ x ++ ".f95") originalFilenames)

				writeFile moduleFileName superKernel_module
				mapM (\(code, filename) -> writeFile filename code) host_programs

extractKernels_beta :: [String] -> (Program Anno, String) -> IO [(String, String)]
extractKernels_beta cppDFlags (ast, filename) = do
				originalLines <- readOriginalFileLines cppDFlags filename
				let originalListing = case originalLines of
										[]	-> ""
										_ -> foldl (\accum item -> accum ++ "\n" ++ item) (head originalLines) (tail originalLines)
				let kernels_code = everything (++) (mkQ [] (synthesiseKernels originalLines (ast, filename))) ast
				let kernels_renamed = map (\(code, kernelname) -> (code, kernelname)) kernels_code
				return kernels_renamed

synthesiseSuperKernelModule :: String -> [(Program Anno, String)] -> [(String, String)] -> String
synthesiseSuperKernelModule moduleName programs kernels = (stateDefinitions ++ kernelModuleHeader ++ (foldl (++) "" kernelCode) ++ superKernelCode ++ kernelModuleFooter)
				where
					kernelCode = map (fst) kernels
					kernelNames = map (snd) kernels
					
					kernelModuleHeader = "module " ++ moduleName ++ "\n\n" ++ tabInc ++ "contains\n\n"
					kernelModuleFooter = "end module " ++ moduleName

					superKernelCode = synthesiseSuperKernel outputTab moduleName programs kernels

					stateNames = map (generateStateName) kernelNames
					-- states = foldl (\accum item -> accum ++ [(DMap.findWithDefault "" item stateMap)]) [] (DMap.keys stateMap)
					stateDefinitions = synthesiseStateDefinitions (zip kernelNames stateNames) 0

generateStateName :: String -> String
generateStateName kernelName = "ST_" ++ (map (toUpper) kernelName)

synthesiseStateDefinitions :: [(String, String)] -> Int -> String
synthesiseStateDefinitions [] currentVal =  "\n"
synthesiseStateDefinitions ((kernel, state):xs) currentVal =  "! " ++ kernel ++ "\n#define " ++ state ++ " " ++ show currentVal ++ "\n" ++ (synthesiseStateDefinitions xs (currentVal+1))

synthesiseSuperKernel :: String -> String -> [(Program Anno, String)] -> [(String, String)] -> (String)
synthesiseSuperKernel tabs name programs kernels = (superKernel)
				where
					programAsts = map (fst) programs
					kernelAstLists = map (extractKernels) programAsts
					-- kernelAstLists = foldl (\accum item -> accum ++ (extractKernels item)) [] programAsts
					kernelAsts = foldl (++) [] (map (\(k_asts, p_ast) -> map (\a -> (a, p_ast)) k_asts) (zip kernelAstLists programAsts))
					kernelArgs = map (\(x, _) -> extractKernelArguments x) kernelAsts
					allKernelArgs = listRemoveDuplications (foldl (++) [] kernelArgs)

					kernelDeclarations = map (\(kernel_ast, prog_ast) -> synthesiseKernelDeclarations prog_ast kernel_ast) kernelAsts
					(readDecls, writtenDecls, generalDecls) = foldl (\(r1,w1,g1) (r2,w2,g2) -> (r1++r2,w1++w2,g1++g2)) ([],[],[]) kernelDeclarations

					readDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" (listRemoveDuplications readDecls)
					writtenDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" (listRemoveDuplications writtenDecls)
					generalDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" (listRemoveDuplications generalDecls)
					statePointerDecl = tabs ++ "integer, dimension(256) :: " ++ (varNameStr stateVarName) ++ "_ptr\n"
					stateAssignment = tabs ++ (varNameStr stateVarName) ++ " = " ++ (varNameStr stateVarName) ++ "_ptr(1)\n"

					superKernel_header = "subroutine " ++ name ++ "(" ++ (foldl (\accum item -> accum ++ "," ++ (varNameStr item)) (varNameStr $ head allKernelArgs) (tail allKernelArgs)) ++ "," ++ (varNameStr stateVarName) ++ "_ptr" ++")\n"
					superKernel_footer = "end subroutine " ++ name ++ "\n"
					superKernel_body = "! SUPERKERNEL BODY\n" ++ selectCase

					kernelNames = map (snd) kernels
					stateNames = map (generateStateName) kernelNames

					caseAlternatives = foldl (\accum (state, name, args) -> accum ++ synthesiseKernelCaseAlternative (tabs ++ outputTab) state name args) "" (zip3 stateNames kernelNames kernelArgs)
					selectCase = outputTab ++ "select case(" ++ (varNameStr stateVarName) ++ ")\n" ++ caseAlternatives ++ outputTab ++ "end select\n"

					superKernel = superKernel_header ++ readDeclStr ++ writtenDeclStr ++ generalDeclStr ++ "\n" ++ statePointerDecl ++ stateAssignment ++ superKernel_body ++ superKernel_footer

synthesiseKernelCaseAlternative :: String -> String -> String -> [VarName Anno] -> String
synthesiseKernelCaseAlternative tabs state kernelName args =  tabs ++ "case (" ++ state ++ ")\n" ++ tabs ++ outputTab ++ kernelName ++ "(" ++ argsString ++ ")" ++ "\n" 
				where
					argsString = foldl (\accum item -> accum ++ "," ++ (varNameStr item)) (varNameStr $ head args) (tail args)

extractKernelArguments :: Fortran Anno -> [VarName Anno]
extractKernelArguments (OpenCLMap _ _ r w _ _) = r ++ w
extractKernelArguments (OpenCLReduce _ _ r w _ rv _) = r ++ w ++ (map (fst) rv)
extractKernelArguments _ = []

--	The strategy taken is one of traversing the AST and emitting any unchanged host code. Where kernels are encountered,
--	calls to the new kernel are emittied. The AST is then traversed again to extract all kernels and then the kernels are
--	emitted.
-- emit :: [String] -> String -> Maybe(String) -> Program Anno -> IO ()
-- emit cppDFlags filename specified ast = do
-- 				let newFilename = case specified of
-- 							Nothing -> defaultFilename (splitOn "/" filename)
-- 							Just spec -> spec
-- 				let kernelModuleNamePath = generateKernelModuleName (splitOn "/" newFilename)
-- 				let kernelModuleName = getModuleName kernelModuleNamePath
				
-- 				originalLines <- readOriginalFileLines cppDFlags filename
-- 				let originalListing = case originalLines of
-- 										[]	-> ""
-- 										_ -> foldl (\accum item -> accum ++ "\n" ++ item) (head originalLines) (tail originalLines)
-- 				let originalFileName = generateOriginalFileName (splitOn "/" newFilename)

-- 				let code = produceCodeProg kernelModuleName originalLines ast
-- 				let kernels = extractKernels originalLines ast
-- 				let kernelModuleHeader = "module " ++ kernelModuleName ++ "\n\n" ++ tabInc ++ "contains\n\n"
-- 				let kernelModuleFooter = "end module " ++ kernelModuleName

-- 				writeFile originalFileName originalListing
-- 				writeFile newFilename code
-- 				writeFile kernelModuleNamePath (kernelModuleHeader ++ kernels ++ kernelModuleFooter)

--	The following functions are used to define names for output files from the input files' names.
getModuleName :: String -> String
getModuleName filename = head (splitOn "." (last (splitOn "/" filename)))

defaultFilename :: [String] -> String
defaultFilename (x:[]) = "par_" ++ x
defaultFilename (x:xs) = x ++ "/" ++ defaultFilename xs

-- generateKernelModuleName :: String -> [String] -> String
-- generateKernelModuleName kernelName (x:[]) = kernelName
-- generateKernelModuleName _ (x:xs) = x ++ "/" ++ generateKernelModuleName xs

-- generateKernelModuleName :: [String] -> String
-- generateKernelModuleName (x:[]) = "module_kernels_" ++ x
-- generateKernelModuleName (x:xs) = x ++ "/" ++ generateKernelModuleName xs

generateOriginalFileName :: [String] -> String
generateOriginalFileName (x:[]) = "original_" ++ x
generateOriginalFileName (x:xs) = x ++ "/" ++ generateOriginalFileName xs

--	This function produces a list of strings where each element is a line of the original source. This
--	list is used heavily in this module.
readOriginalFileLines :: [String] -> String -> IO ([String])
readOriginalFileLines cppDFlags filename = do
				content <- cpp cppDFlags filename
				let contentLines = lines content
				return contentLines

-- extractKernels :: [String] -> Program Anno -> [(String, String)]
-- -- extractKernels :: [String] -> Program Anno -> String
-- extractKernels originalLines prog  = everything (++) (mkQ [] (synthesiseKernels originalLines prog)) prog

extractKernels :: Program Anno -> [Fortran Anno]
extractKernels ast = everything (++) (mkQ [] (extractKernels')) ast

extractKernels' :: Fortran Anno -> [Fortran Anno]
extractKernels' codeSeg = case codeSeg of
							OpenCLMap _ _ _ _ _ _ -> [codeSeg]
							OpenCLReduce _ _ _ _ _ _ _ -> [codeSeg]
							_ -> []

-- synthesiseKernels :: [String] -> Program Anno -> Fortran Anno -> String
synthesiseKernels :: [String] -> (Program Anno, String) -> Fortran Anno -> [(String, String)]
synthesiseKernels originalLines prog codeSeg = case codeSeg of
				OpenCLMap _ src _ w _ _ -> [(synthesiseOpenCLMap "" originalLines prog codeSeg, generateKernelName "map" src w)]
				OpenCLReduce _ src _ _ _ rv _ ->  [(synthesiseOpenCLReduce "" originalLines prog codeSeg, generateKernelName "reduce" src (map (\(v, e) -> v) rv))]
				_ -> []

synthesiseKernelDeclarations :: Program Anno -> Fortran Anno -> ([Decl Anno], [Decl Anno], [Decl Anno])
synthesiseKernelDeclarations prog (OpenCLMap _ _ r w _ _) = (readDecls, writtenDecls, generalDecls)
				where
					readArgs = listSubtract r w
					writtenArgs = listSubtract w r
					generalArgs = listIntersection w r
					
					readDecls = map (\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (In nullAnno) prog)) readArgs
					writtenDecls = map (\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (Out nullAnno) prog)) writtenArgs
					generalDecls = map (\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (InOut nullAnno) prog)) generalArgs
synthesiseKernelDeclarations prog (OpenCLReduce _ _ r w _ rv _) = (readDecls, writtenDecls, generalDecls)
				where
					reductionVarNames = map (\(varname, expr) -> varname) rv
					readArgs = listSubtract (listSubtract r w) reductionVarNames
					writtenArgs = listSubtract (listSubtract w r) reductionVarNames
					generalArgs = listSubtract (listIntersection w r) reductionVarNames

					readDecls = map (\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (In nullAnno) prog)) readArgs
					writtenDecls = map (\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (Out nullAnno) prog)) writtenArgs
					generalDecls = map (\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (InOut nullAnno) prog)) generalArgs

produceCodeProg :: [String] -> String -> (Program Anno, String) -> IO(String)
produceCodeProg cppDFlags kernelModuleName (prog, filename) = do
					originalLines <- readOriginalFileLines cppDFlags filename
					let result = foldl (\accum item -> accum ++ produceCodeProgUnit (prog, filename) kernelModuleName originalLines item) "" prog
					return result
-- produceCodeProg :: String -> [String] -> Program Anno -> String
-- produceCodeProg kernelModuleName originalLines prog = foldl (\accum item -> accum ++ produceCodeProgUnit kernelModuleName originalLines item) "" prog

--	This function handles producing the code that is not changed from the original source. It also inserts a "use .."
--	line for the file containing the new kernels and makes a call to the function that produces the body of the new
--	host code. 
produceCodeProgUnit :: (Program Anno, String) -> String -> [String] -> ProgUnit Anno -> String
produceCodeProgUnit prog kernelModuleName originalLines progUnit = nonGeneratedHeaderCode 
												 ++ tabInc ++ "use " ++ kernelModuleName ++ "\n" 
												 ++ nonGeneratedBlockCode 
												 ++ everything (++) (mkQ "" (produceCodeBlock prog originalLines)) progUnit
												 ++ nonGeneratedFooterCode			
							where
								progUnitSrc = srcSpan progUnit
								firstFortranSrc = head (everything (++) (mkQ [] (getFirstFortranSrc)) progUnit)
								firstBlockSrc = head (everything (++) (mkQ [] (getFirstBlockSrc)) progUnit)
								(nonGeneratedHeaderSrc, nonGeneratedFooterSrc) = getSrcSpanNonIntersection progUnitSrc firstBlockSrc

								((SrcLoc _ nonGeneratedHeader_ls _), (SrcLoc _ nonGeneratedHeader_le _)) = nonGeneratedHeaderSrc
								nonGeneratedHeaderCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedHeader_ls..nonGeneratedHeader_le-1]

								((SrcLoc _ nonGeneratedFooter_ls _), (SrcLoc _ nonGeneratedFooter_le _)) = nonGeneratedFooterSrc
								nonGeneratedFooterCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedFooter_ls-1..nonGeneratedFooter_le-1]

								((SrcLoc _ block_ls _), (SrcLoc _ _ _)) = firstBlockSrc
								((SrcLoc _ fortran_ls _), (SrcLoc _ _ _)) = firstFortranSrc
								nonGeneratedBlockCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [block_ls..fortran_ls-1]

--	Function is used (along with "getFirstBlockSrc") by "produceCodeProgUnit" to determine which lines of the original
--	source can be taken as is. It is used to determine where the first Fortran nodes of the AST appear in the source
--	because the Fortran nodes are the ones that have been transformed.
getFirstFortranSrc :: Block Anno -> [SrcSpan]
getFirstFortranSrc (Block _ _ _ _ _ fortran) = [srcSpan fortran]

getFirstBlockSrc :: Block Anno -> [SrcSpan]
getFirstBlockSrc codeSeg = [srcSpan codeSeg]

produceCodeBlock :: (Program Anno, String) -> [String] -> Block Anno -> String
produceCodeBlock prog originalLines block = foldl (++) "" (gmapQ (mkQ "" (produceCode_fortran prog "" originalLines)) block)

--	This function is called very often. It is the default when producing the body of each of the kernels and calls other functions
--	based on the node in the AST that it is called against. Each of the 'synthesise...' functions check whether the node in question
--	is a 'generated' node to determine whether or not code from the original file can be used.
produceCode_fortran :: (Program Anno, String) -> String -> [String] -> Fortran Anno -> String
produceCode_fortran prog tabs originalLines codeSeg = case codeSeg of
						If _ _ _ _ _ _ -> synthesiseIf prog tabs originalLines codeSeg
						Assg _ _ _ _ -> synthesiseAssg prog tabs originalLines codeSeg
						For _ _ _ _ _ _ _ -> synthesiseFor prog tabs originalLines codeSeg
						NullStmt _ _ -> ""
						OpenCLMap _ _ _ _ _ _ -> (generateKernelCall prog tabs codeSeg) ++ (commentSeparator "END")
						OpenCLReduce _ _ _ _ _ rv f -> (generateKernelCall prog tabs codeSeg)
																++ (mkQ "" (produceCode_fortran prog "" originalLines) hostReductionLoop) ++ "\n" ++ (commentSeparator "END")
								where 
									reductionVarNames = map (\(varname, expr) -> varname) rv
									r_iter = generateReductionIterator reductionVarNames
									hostReduction = generateFinalHostReduction reductionVarNames r_iter f
									hostReductionLoop = generateLoop r_iter (generateIntConstant 1) nunitsVar hostReduction
									
						FSeq _ _ fortran1 fortran2 -> (mkQ "" (produceCode_fortran prog tabs originalLines) fortran1) ++ (mkQ "" (produceCode_fortran prog tabs originalLines) fortran2)
						_ -> 	case anyChildGenerated codeSeg || isGenerated codeSeg of
									True -> foldl (++) tabs (gmapQ (mkQ "" (produceCode_fortran prog "" originalLines)) codeSeg)
									False -> extractOriginalCode tabs originalLines (srcSpan codeSeg)

synthesisUses :: String -> Uses Anno -> String
synthesisUses tabs (Use _ (str, rename) _ _) = tabs ++ "use " ++ str ++ "\n"
synthesisUses tabs _ = ""

synthesiseFor :: (Program Anno, String) -> String -> [String] -> Fortran Anno -> String
synthesiseFor prog tabs originalLines (For anno src varname expr1 expr2 expr3 fort) 	|	partialGenerated = tabs ++ "do " ++ (varNameStr varname) ++ "=" ++ outputExprFormatting expr1 
																								++ ", " ++ outputExprFormatting expr2 ++ (if expr3isOne then "" else outputExprFormatting expr3)
																								++ "\n" ++ (mkQ "" (produceCode_fortran prog (tabs ++ tabInc) originalLines) fort) ++ tabs ++ "end do\n"
																				|	otherwise = extractOriginalCode tabs originalLines src
																	where
																		expr3isOne = case expr3 of
																						Con _ _ "1" -> True
																						_ -> False
																		partialGenerated = anyChildGenerated codeSeg || isGenerated codeSeg
																		codeSeg = For anno src varname expr1 expr2 expr3 fort

synthesiseAssg :: (Program Anno, String) -> String -> [String] -> Fortran Anno -> String
synthesiseAssg prog tabs originalLines (Assg anno src expr1 expr2)	|	partialGenerated = tabs ++ outputExprFormatting expr1 ++ " = " ++ outputExprFormatting expr2 ++ "\n"
															|	otherwise = extractOriginalCode tabs originalLines src
											where 
												partialGenerated = isGenerated codeSeg
												codeSeg = Assg anno src expr1 expr2

synthesiseIf :: (Program Anno, String) -> String -> [String] -> Fortran Anno -> String
synthesiseIf prog tabs originalLines (If anno src expr fortran lst maybeFort) 	|	partialGenerated = tabs ++ "If (" ++ outputExprFormatting expr ++ ") then\n" 
																	++ mainFortranStr
																	++ elseIfFortranStr
																	++ elseFortranStr
																	++ tabs ++ "end if\n"
															|	otherwise = extractOriginalCode tabs originalLines src
											where 
												partialGenerated = anyChildGenerated codeSeg || isGenerated codeSeg
												codeSeg = If anno src expr fortran lst maybeFort
												mainFortranStr = (mkQ "" (produceCode_fortran prog (tabs ++ tabInc) originalLines) fortran)
												elseIfFortranStr = foldl (synthesisElses prog tabs originalLines) "" lst
												elseFortranStr = case maybeFort of
																	Just a -> (mkQ "" (produceCode_fortran prog (tabs ++ tabInc) originalLines) a)
																	Nothing -> ""

synthesisElses :: (Program Anno, String) -> String -> [String] -> String -> (Expr Anno, Fortran Anno) -> String
synthesisElses prog tabs originalLines accum (expr, fortran) = accum ++ tabs ++ "else if(" ++ outputExprFormatting expr ++ ") then\n" 
																++ (mkQ "" (produceCode_fortran prog (tabs ++ tabInc) originalLines) fortran)
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

--	Function handles the construction of map kernels. The body of the kernel is the original source that appeared in the loop that
--	has been parallelised. The extra elements are initialising OpenCL related constructs and wrapping subroutine syntax.
synthesiseOpenCLMap :: String -> [String] -> (Program Anno, String) -> Fortran Anno -> String
synthesiseOpenCLMap inTabs originalLines programInfo (OpenCLMap anno src r w l fortran) =
																	inTabs ++ "subroutine " ++ kernelName
																	++"(" 
																					++ allArgsStr ++ ")\n"
																	++ usesString
																	++ "\n"
																	++ readDeclStr
																	++ writtenDeclStr
																	++ generalDeclStr
																	++ tabs ++ globalIdDeclaration
																	++ tabs ++ globalIdInitialisation
																	++ "\n"
																	++ tabs ++ "! " ++ compilerName ++ ": Synthesised loop variables\n"
																	++ produceCode_fortran programInfo (tabs) originalLines loopInitialiserCode 
																	++ "\n\n"
																	++ tabs ++ "! " ++ compilerName ++ ": Original code\n" 
																	++ (mkQ "" (produceCode_fortran programInfo (tabs) originalLines) fortran) 
																	++ "\n"
																	++ inTabs ++ "end subroutine " ++ kernelName
																	++ "\n\n\n"

											where
												extractedUses = everything (++) (mkQ [] getUses) prog
												usesString = foldl (\accum item -> accum ++ synthesisUses tabs item) "" extractedUses 

												prog = fst programInfo

												kernelName = generateKernelName "map" src w
												globalIdVar = generateVar (VarName nullAnno "global_id")
												tabs = inTabs ++ tabInc

												readArgs = listSubtract r w
												writtenArgs = listSubtract w r
												generalArgs = listIntersection w r

												allArgs = readArgs ++ writtenArgs ++ generalArgs
												allArgsStr = case allArgs of
															[] -> ""
															args -> foldl (\accum item -> accum ++ "," ++ varNameStr item) (varNameStr (head args)) (tail args)
												
												-- readDecls = map (\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (In nullAnno) prog)) readArgs
												-- writtenDecls = map (\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (Out nullAnno) prog)) writtenArgs
												-- generalDecls = map (\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (InOut nullAnno) prog)) generalArgs
												
												(readDecls, writtenDecls, generalDecls) = synthesiseKernelDeclarations prog (OpenCLMap anno src r w l fortran)
												-- readDecls = map (\x ->fromMaybe (NullDecl nullAnno nullSrcSpan) (adaptOriginalDeclaration_intent x (In nullAnno) prog)) readArgs
												-- writtenDecls = map (\x ->fromMaybe (NullDecl nullAnno nullSrcSpan) (adaptOriginalDeclaration_intent x (Out nullAnno) prog)) writtenArgs
												-- generalDecls = map (\x ->fromMaybe (NullDecl nullAnno nullSrcSpan) (adaptOriginalDeclaration_intent x (InOut nullAnno) prog)) generalArgs

												readDeclStr = foldl (\accum item -> accum ++ synthesiseDecl (tabs) item) "" (readDecls)
												writtenDeclStr = foldl (\accum item -> accum ++ synthesiseDecl (tabs) item) "" (writtenDecls)
												generalDeclStr = foldl (\accum item -> accum ++ synthesiseDecl (tabs) item) "" (generalDecls)

												globalIdDeclaration = "integer :: " ++ outputExprFormatting globalIdVar ++ "\n"
												globalIdInitialisation = "call " ++ outputExprFormatting (getGlobalID globalIdVar) ++ "\n"

												loopInitialisers = generateLoopInitialisers l globalIdVar Nothing
												loopInitialiserCode = case loopInitialisers of
																		[] -> error "synthesiseOpenCLMap: loopInitialiserCode - empty list"
																		_ -> foldl1 (\accum item -> appendFortran_recursive item accum) loopInitialisers

--	Function handles the production of OpenCL reduction kernels. Clearly it is much more complicated the the 'synthesiseOpenCLMap' function because the final reduction
--	kernel need be smarter than a map.

--	BASIC STEPS
--	-	Produce subroutine header with appropriate arguments 
--	-	Deteremine imports and produce 'using' statements 
--	-	Produce declarations for new variables that are introduced for OpenCL's sake.
--	-	Get declarations for the existing variables from the original srouce
--	-	Declare variables that are used directly by the tree based reduction
--	-	Initialise OpenCL related variables like 'local_id'
--	-	Produce the first reduction loop, the one that happens per thread. The body of this loop is functionally the same code that appeared
--		in the original source.
--	-	Produce synchronisation barrier
--	-	Produce second reduction loop, computer unit loop. Functionality is adapted from original code and only includes the assosciative,
--		primary reduction operation.
--	-	Assign values to global result array that host will reduce later
--	-	End subroutine. 
synthesiseOpenCLReduce :: String ->  [String] -> (Program Anno, String)-> Fortran Anno -> String
synthesiseOpenCLReduce inTabs originalLines programInfo (OpenCLReduce anno src r w l rv fortran)  =
																			inTabs ++ "subroutine " ++ kernelName
																			++ "(" 				
																			++ allArgsStr
																			++ ")\n"
																			++ usesString
																			++ "\n"
																			++ tabs ++ chunk_sizeDeclaration
																			++ tabs ++ localIdDeclaration
																			++ tabs ++ localIdFortranDeclaration
																			++ tabs ++ groupIdDeclaration
																			++ tabs ++ groupIdFortranDeclaration
																			++ tabs ++ globalIdDeclaration 
																			++ tabs ++ reductionIteratorDeclaration
																			++ tabs ++ localChunkSizeDeclaration
																			++ tabs ++ startPositionDeclaration
																			++ "\n"
																			++ readDeclStr
																			++ writtenDeclStr
																			++ generalDeclStr
																			++ "\n"
																			++ tabs ++ "! Arrays prefixed with \'local_\' should be declared using the \'__local\' modifier in C kernel version\n"
																			++ workGroup_reductionArraysDeclStr
																			++ global_reductionArraysDeclStr
																			++ local_reductionVarsDeclatationStr
																			++ "\n"
																			++ tabs ++ localIdInitialisation
																			++ tabs ++ groupIdInitialisation
																			++ tabs ++ globalIdInitialisation
																			++ "\n" ++ tabs ++ "! local_id_fortran and group_id_fortran are used to reconcile the fact that fortran arrays are referenced from 1"
																			++ "\n" ++ tabs ++ "! not 0 like other OpenCL supporting languages\n"
																			++ tabs ++ localIdFortranInitialisation
																			++ tabs ++ groupIdFortranInitialisation
																			++ tabs ++ localChunkSize_str
																			++ tabs ++ startPosition_str
																			++ local_reductionVarsInitStr
																			++ "\n"
																			++ (mkQ "" (produceCode_fortran programInfo (tabs) originalLines) workItem_loop)
																			++ "\n"
																			++ workGroup_reductionArraysInitStr
																			++ "\n"
																			++ tabs ++ localMemBarrier
																			++ "\n"
																			++ local_reductionVarsInitStr
																			++ (mkQ "" (produceCode_fortran programInfo (tabs) originalLines) workGroup_loop)
																			++ global_reductionArraysAssignmentStr
																			++ "\n"
																			++ inTabs ++ "end subroutine " ++ kernelName
																			++"\n\n\n"
											where
												prog = fst programInfo

												kernelName = generateKernelName "reduce" src (map (\(v, e) -> v) rv)
												tabs = inTabs ++ tabInc

												extractedUses = everything (++) (mkQ [] getUses) prog
												usesString = foldl (\accum item -> accum ++ synthesisUses tabs item) "" extractedUses 

												reductionVarNames = map (\(varname, expr) -> varname) rv
												readVarNames = listSubtract (listSubtract r w) reductionVarNames
												writtenVarNames = listSubtract (listSubtract w r) reductionVarNames
												generalVarNames = listSubtract (listIntersection w r) reductionVarNames

												localIdVar = generateVar (VarName nullAnno "local_id")
												localIdFortranVar = generateVar (VarName nullAnno "local_id_fortran")
												groupIdVar = generateVar (VarName nullAnno "group_id")
												groupIdFortranVar = generateVar (VarName nullAnno "group_id_fortran")
												globalIdVar = generateVar (VarName nullAnno "global_id")

												localIdDeclaration = "integer :: " ++ outputExprFormatting localIdVar ++ "\n"
												localIdFortranDeclaration = "integer :: " ++ outputExprFormatting localIdFortranVar ++ "\n"
												groupIdDeclaration = "integer :: " ++ outputExprFormatting groupIdVar ++ "\n"
												groupIdFortranDeclaration = "integer :: " ++ outputExprFormatting groupIdFortranVar ++ "\n"
												globalIdDeclaration = "integer :: " ++ outputExprFormatting globalIdVar ++ "\n"
												reductionIteratorDeclaration = "integer :: " ++ varNameStr reductionIterator ++ "\n"
												localChunkSizeDeclaration = "integer :: " ++ outputExprFormatting localChunkSize ++ "\n"
												startPositionDeclaration = "integer :: " ++ outputExprFormatting startPosition ++ "\n"
												chunk_sizeDeclaration = "integer :: " ++ outputExprFormatting chunk_size ++ "\n"

												localIdInitialisation = "call " ++ outputExprFormatting (getLocalId localIdVar) ++ "\n"
												localIdFortranInitialisation = synthesiseAssg programInfo inTabs originalLines (generateAssgCode localIdFortranVar (generateAdditionExpr localIdVar (generateIntConstant 1)))
												groupIdInitialisation = "call " ++ outputExprFormatting (getGroupID groupIdVar) ++ "\n"
												groupIdFortranInitialisation = synthesiseAssg programInfo inTabs originalLines (generateAssgCode groupIdFortranVar (generateAdditionExpr groupIdVar (generateIntConstant 1)))
												globalIdInitialisation = "call " ++ outputExprFormatting (getGlobalID globalIdVar) ++ "\n"
												groupSizeInitialisation_calculation = generateGlobalWorkItemsExpr l

												allArgs = readVarNames ++ writtenVarNames ++ generalVarNames ++ global_reductionArrays
												allArgsStr = case allArgs of
															[] -> ""
															args -> foldl (\accum item -> accum ++ "," ++ varNameStr item) (varNameStr (head args)) (tail args)
												
												-- readDecls = removeDeclAssignments $ map (\x -> fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (In nullAnno) prog)) readVarNames
												-- writtenDecls = removeDeclAssignments $ map (\x -> fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (Out nullAnno) prog)) writtenVarNames
												-- generalDecls = removeDeclAssignments $ map (\x -> fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (InOut nullAnno) prog)) generalVarNames
												(readDecls, writtenDecls, generalDecls) = synthesiseKernelDeclarations prog (OpenCLReduce anno src r w l rv fortran)
												-- readDecls = removeDeclAssignments $ map (\x -> fromMaybe (NullDecl nullAnno nullSrcSpan) (adaptOriginalDeclaration_intent x (In nullAnno) prog)) readVarNames
												-- writtenDecls = removeDeclAssignments $ map (\x -> fromMaybe (NullDecl nullAnno nullSrcSpan) (adaptOriginalDeclaration_intent x (Out nullAnno) prog)) writtenVarNames
												-- generalDecls = removeDeclAssignments $ map (\x -> fromMaybe (NullDecl nullAnno nullSrcSpan) (adaptOriginalDeclaration_intent x (InOut nullAnno) prog)) generalVarNames

												readDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" (readDecls)
												writtenDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" (writtenDecls)
												generalDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" (generalDecls)
												
												local_reductionVars = map (generateLocalReductionVar) reductionVarNames
												local_reductionVarsInitStr = foldl (\accum (var, expr) -> accum ++ tabs ++ "local_" ++ varNameStr var ++ " = " ++ outputExprFormatting expr ++ "\n") "" rv
												local_reductionVarsDeclatation = map (\(red, local) -> stripDeclAttrs $ adaptOriginalDeclaration_varname red local prog) (zip reductionVarNames local_reductionVars)
												-- local_reductionVarsDeclatation = map (\(red, local) -> stripDeclAttrs $ fromMaybe (NullDecl nullAnno nullSrcSpan) (adaptOriginalDeclaration_varname red local prog)) (zip reductionVarNames local_reductionVars)
												local_reductionVarsDeclatationStr = synthesiseDecls tabs local_reductionVarsDeclatation

												localChunkSize_assg = generateAssgCode 
																			localChunkSize 
																			(generateDivisionExpr
																				(generateDivisionExpr 
																					(generateGlobalWorkItemsExpr l)
																					nthVar)
																				nunitsVar)
												localChunkSize_str = synthesiseAssg programInfo inTabs originalLines localChunkSize_assg

												startPosition_str = (outputExprFormatting startPosition) ++ " = " ++ (outputExprFormatting localChunkSize) ++
													" * " ++ (outputExprFormatting globalIdVar) ++ "\n"

												reductionIterator = generateReductionIterator (r ++ w ++ (map (\(x,_,_,_) -> x) l) ++ reductionVarNames)
												workItem_loopEnd = generateSubtractionExpr (generateAdditionExpr startPosition localChunkSize) (generateIntConstant 1)
												workItem_loopCode = appendFortran_recursive workItem_reductionCode workItem_loopInitialiserCode 
												workItem_loop = generateLoop reductionIterator startPosition workItem_loopEnd workItem_loopCode
												workItem_reductionCode = applyGeneratedSrcSpans (replaceAllOccurences_varnamePairs fortran reductionVarNames local_reductionVars)

												workItem_loopInitialisers = generateLoopInitialisers l (generateVar reductionIterator) Nothing
												workItem_loopInitialiserCode = case workItem_loopInitialisers of
																				[] -> error "synthesiseOpenCLReduce: workItem_loopInitialiserCode - empty list"
																				_ -> foldl1 (\accum item -> appendFortran_recursive item accum) workItem_loopInitialisers

												workGroup_reductionArrays = map (generateLocalReductionArray) reductionVarNames
												workGroup_reductionArraysDecl = map (\x -> declareLocalReductionArray x (nthVar) prog) reductionVarNames
												-- workGroup_reductionArraysDecl = map (\x -> fromMaybe (NullDecl nullAnno nullSrcSpan) (declareLocalReductionArray x (nthVar) prog)) reductionVarNames
												workGroup_reductionArraysDeclStr = synthesiseDecls tabs workGroup_reductionArraysDecl
												workGroup_reductionArraysInitStr = foldl (generateReductionArrayAssignment tabs localIdFortranVar) "" (zip workGroup_reductionArrays local_reductionVars)
												workGroup_reductionCode = generateWorkGroupReduction reductionVarNames reductionIterator fortran
												workGroup_loop = generateLoop reductionIterator (generateIntConstant 1) nthVar workGroup_reductionCode


												global_reductionArrays = map (generateGlobalReductionArray) reductionVarNames
												global_reductionArraysDecl = map (\x -> declareGlobalReductionArray x (nunitsVar) prog) reductionVarNames
												-- global_reductionArraysDecl = map (\x -> fromMaybe (NullDecl nullAnno nullSrcSpan) (declareGlobalReductionArray x (nunitsVar) prog)) reductionVarNames
												global_reductionArraysDeclStr = synthesiseDecls tabs global_reductionArraysDecl
												global_reductionArraysAssignmentStr = foldl (generateReductionArrayAssignment tabs groupIdFortranVar) "" (zip global_reductionArrays local_reductionVars)

localChunkSize = generateVar (VarName nullAnno "local_chunk_size")
startPosition  = generateVar (VarName nullAnno "start_position")
chunk_size = generateVar chunk_size_varname
chunk_size_varname = VarName nullAnno "chunk_size"
localMemBarrier = "call barrier(CLK_LOCAL_MEM_FENCE)\n"
nthVar = generateVar (VarName nullAnno "NTH")
nunitsVar = generateVar (VarName nullAnno "NUNITS")
numGroupsVarName = VarName nullAnno "num_groups"
numGroupsVar = generateVar numGroupsVarName
stateVarName = VarName nullAnno "state"
stateVar = generateVar stateVarName


generateLocalReductionArray (VarName anno str) = VarName anno ("local_" ++ str ++ "_array")
generateGlobalReductionArray (VarName anno str) = VarName anno ("global_" ++ str ++ "_array")
generateLocalReductionArrayArgStr accum item = accum ++ "\n" ++ tabInc ++ "__local " ++ varNameStr item
generateGloablReductionArrayArgStr accum item = accum ++ "\n" ++ tabInc ++ "__global " ++ varNameStr item
generateLocalReductionVar (VarName anno str) = VarName anno ("local_" ++ str)

generateReductionArrayAssignment tabs accessor accum ((VarName _ s1),(VarName _ s2)) = accum++tabs++s1++"("++(outputExprFormatting accessor)++") = "++s2++"\n"

-- Decl           p SrcSpan [(Expr p, Expr p, Maybe Int)] (Type p)
-- BaseType p                    (BaseType p) [Attr p] (Expr p) (Expr p)
generateImplicitDecl :: VarName Anno -> Decl Anno
generateImplicitDecl var = Decl nullAnno nullSrcSpan [(generateVar var, (NullExpr nullAnno nullSrcSpan), Nothing)] (BaseType nullAnno (Real nullAnno) [] (NullExpr nullAnno nullSrcSpan) (NullExpr nullAnno nullSrcSpan))

generateKernelName :: String -> SrcSpan -> [VarName Anno] -> String
generateKernelName identifier src varnames = (getModuleName filename) ++ "_" ++ identifier
											++ (foldl (\accum item -> accum ++ "_" ++ (varNameStr item)) "" varnames) 
											++ "_" ++ show line
			where
				((SrcLoc filename line _), _) = src

--	Function used during host code generation to produce call to OpenCL kernel.
generateKernelCall :: (Program Anno, String) -> String -> Fortran Anno -> String
generateKernelCall (progAst, filename) tabs (OpenCLMap anno src r w l fortran) = 	-- "! Global work items: " ++ outputExprFormatting globalWorkItems ++ "\n"
														(commentSeparator ("BEGIN " ++ kernelName))
														++ tabs ++ "oclGlobalRange = " ++ outputExprFormatting globalWorkItems ++ "\n"
														++ tabs ++ "state = " ++ stateName ++ "\n"
														++ tabs ++ bufferWrites ++ "\n"
														++ tabs ++ "call runOcl(oclGlobalRange,oclLocalRange,exectime)\n"
														++ tabs ++ "! call " ++ kernelName
														++ tabs ++ "(" ++ allArgumentsStr ++ ")"++ "" ++ tabInc ++ "! Call to synthesised, external kernel\n"
														++ tabs ++ bufferReads ++ "\n\n"
			where
				readArgs = map (varNameStr) (listSubtract r w)
				writtenArgs = map (varNameStr) (listSubtract w r)
				generalArgs = map (varNameStr) (listIntersection w r)

				allArguments = readArgs ++ writtenArgs ++ generalArgs
				allArgumentsStr = (head allArguments) ++ foldl (\accum item -> accum ++ "," ++ item) "" (tail allArguments)

				globalWorkItems = generateGlobalWorkItemsExpr l

				kernelName = (generateKernelName "map" src w)
				stateName = generateStateName kernelName

				bufferWrites = foldl (\accum item -> accum ++ "\n" ++ item) "" (map (synthesiseBufferAccess "Write") (readDecls ++ generalDecls))
				bufferReads = foldl (\accum item -> accum ++ "\n" ++ item) "" (map (synthesiseBufferAccess "Read") (writtenDecls ++ generalDecls))

				(readDecls, writtenDecls, generalDecls) = synthesiseKernelDeclarations progAst (OpenCLMap anno src r w l fortran)
				


generateKernelCall (progAst, filename) tabs (OpenCLReduce anno src r w l rv fortran) = 	-- "\n! Global work items: " ++ outputExprFormatting reductionWorkItemsExpr ++ "\n"
															(commentSeparator ("BEGIN " ++ kernelName))
															++ tabs ++ "oclGlobalRange = " ++ outputExprFormatting reductionWorkItemsExpr ++ "\n"
															++ tabs ++ "oclLocalRange = " ++ outputExprFormatting nthVar ++ "\n"
															++ tabs ++ "ngroups = " ++ outputExprFormatting nunitsVar ++ "\n"
															++ tabs ++ "state = " ++ stateName ++ "\n"
															++ tabs ++ bufferWrites ++ "\n\n"
															++ tabs ++ "call runOcl(oclGlobalRange,oclLocalRange,exectime)\n"
															++ tabs ++ "! call " ++ kernelName
															++ tabs ++ "(" ++ allArgumentsStr ++ ")" ++ "" ++ tabInc ++ "! Call to synthesised, external kernel\n"
															++ tabs ++ bufferReads
															++ tabs ++ bufferReads_rv ++ "\n\n"
			where 
				reductionVarNames = map (\(varname, expr) -> varname) rv
				workGroup_reductionArrays = map (generateLocalReductionArray) reductionVarNames
				global_reductionArrays = map (generateGlobalReductionArray) reductionVarNames
				readArgs = listSubtract r w
				writtenArgs = listSubtract w r
				generalArgs = listIntersection w r

				allArguments = 	(map (varNameStr) 
									(listSubtract 
										(readArgs ++ writtenArgs ++ generalArgs ++ global_reductionArrays) 
										reductionVarNames)) 
				allArgumentsStr =  (head allArguments) ++ foldl (\accum item -> accum ++ "," ++ item) "" (tail allArguments)

				reductionWorkItemsExpr = generateProductExpr nthVar nunitsVar

				kernelName = (generateKernelName "reduce" src (map (\(v, e) -> v) rv)) 
				stateName = generateStateName kernelName

				bufferWrites = foldl (\accum item -> accum ++ "\n" ++ item) "" (map (synthesiseBufferAccess "Write") (readDecls ++ generalDecls))
				bufferReads = foldl (\accum item -> accum ++ "\n" ++ item) "" (map (synthesiseBufferAccess "Read") (writtenDecls ++ generalDecls))
				bufferReads_rv = foldl (\accum item -> accum ++ "\n" ++ item) "" (map (synthesiseBufferAccess "Read") (global_reductionArraysDecls))

				-- bufferReads_rv = bufferReads ++ (foldl (\accum item -> accum ++ "\n" ++ item) "" (map (\x -> "call oclRead2D_type_ArrayBuffer(" ++ varNameStr x ++ ")") global_reductionArrays ))

				(readDecls, writtenDecls, generalDecls) = synthesiseKernelDeclarations progAst (OpenCLReduce anno src r w l rv fortran)

				global_reductionArraysDecls = map (\x -> declareGlobalReductionArray x (nunitsVar) progAst) reductionVarNames

synthesiseBufferAccess :: String -> Decl Anno -> String
synthesiseBufferAccess method (Decl anno src lst typ) = case baseType of
														Integer _ -> prefix ++ "Int" ++ suffix
														Real _ -> prefix ++ "Float" ++ suffix
														_ -> ""
			where
				varStr = varNameStr $ extractAssigneeFromDecl (Decl anno src lst typ)
				baseType = extractBaseType typ
				dimensionList = (everything (++) (mkQ [] extractDimensionAttr) (Decl anno src lst typ))
				dimensions = case dimensionList of
									[] -> 1
									_ -> dimensionSize $ head dimensionList
				prefix = "call ocl" ++ method ++ (show dimensions) ++ "D"
				suffix = "ArrayBuffer(" ++ varStr ++ "_buf," ++ varStr ++ "_sz," ++ varStr ++ ")"
synthesiseBufferAccess _ _ = error "synthesiseBufferAccess"

dimensionSize :: Attr Anno -> Int
dimensionSize (Dimension _ lst) = length lst
dimensionSize _ = 0

generateLoopIterationsExpr :: (VarName Anno, Expr Anno, Expr Anno, Expr Anno) -> Expr Anno
generateLoopIterationsExpr (var, (Con _ _ "1"), end, (Con _ _ "1")) = end
generateLoopIterationsExpr (var, start, end, (Con _ _ "1")) = (Bin nullAnno nullSrcSpan (Plus nullAnno)
																(generateSubtractionExpr end start) 
																(generateIntConstant 1))
generateLoopIterationsExpr (var, (Con _ _ "1"), end, step) = Bin nullAnno nullSrcSpan (Div nullAnno) 
																end
																step
generateLoopIterationsExpr (var, start, end, step) = Bin nullAnno nullSrcSpan (Div nullAnno) 
														(Bin nullAnno nullSrcSpan (Plus nullAnno)
															(generateSubtractionExpr_list [end, start]) 
															(generateIntConstant 1))
														step

declareGlobalReductionArray :: VarName Anno -> Expr Anno -> Program Anno -> Decl Anno -- Maybe(Decl Anno)
declareGlobalReductionArray varname arraySize program = decl
														-- case decl_list of
														-- [] -> Nothing
														-- _ -> Just (decl)
			where
				decl_list = everything (++) (mkQ [] (extractDeclaration_varname varname)) program
				foundDecl = case decl_list of
								[] -> generateImplicitDecl varname
								_ -> head decl_list
				one = generateIntConstant 1
				newVarName = generateGlobalReductionArray varname
				decl = applyIntent (Out nullAnno) (replaceAllOccurences_varname (addDimension foundDecl one arraySize) varname newVarName)
				-- decl = applyIntent (Out nullAnno) (replaceAllOccurences_varname (addDimension (head decl_list) one arraySize) varname newVarName)

declareLocalReductionArray :: VarName Anno -> Expr Anno -> Program Anno -> Decl Anno -- Maybe(Decl Anno)
declareLocalReductionArray varname arraySize program = decl
														-- case decl_list of
														-- [] -> Nothing
														-- _ -> Just (decl)
			where
				decl_list = everything (++) (mkQ [] (extractDeclaration_varname varname)) program
				foundDecl = case decl_list of
								[] -> generateImplicitDecl varname
								_ -> head decl_list
				one = generateIntConstant 1
				newVarName = generateLocalReductionArray varname
				decl = addDimension (replaceAllOccurences_varname foundDecl varname newVarName) one arraySize
				-- decl = addDimension (replaceAllOccurences_varname (head decl_list) varname newVarName) one arraySize

adaptOriginalDeclaration_varname :: VarName Anno -> VarName Anno -> Program Anno -> Decl Anno -- Maybe(Decl Anno)
adaptOriginalDeclaration_varname varname newVarname program = decl
														-- case decl_list of
														-- [] -> Nothing
														-- _ -> Just (decl)
			where 
				decl_list = everything (++) (mkQ [] (extractDeclaration_varname varname)) program
				foundDecl = case decl_list of
								[] -> generateImplicitDecl varname
								_ -> head decl_list
				decl = applyGeneratedSrcSpans (replaceAllOccurences_varname foundDecl varname newVarname)
				-- decl = applyGeneratedSrcSpans (replaceAllOccurences_varname (head decl_list) varname newVarname)



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
appendDimension start end att = att

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
				decl_list = everything (++) (mkQ [] (extractDeclaration_varname varname)) program
				decl = case containsParameterAttr (head decl_list) of
							True -> applyGeneratedSrcSpans (head decl_list)
							False -> applyGeneratedSrcSpans (applyIntent (intent) (head decl_list))

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

extractDeclaration_varname :: VarName Anno -> Decl Anno -> [Decl Anno]
extractDeclaration_varname varname  (Decl anno src lst typ)  	| firstHasVar || secondHasVar = [Decl anno src lst typ]
														| otherwise = []
			where
				firstExprs = map (\(expr, _, _) -> expr) lst
				secondExprs = map (\(_, expr, _) -> expr) lst

				firstHasVar = foldl (\accum item -> accum || usesVarName_list [varname] item) False firstExprs
				secondHasVar = foldl (\accum item -> accum || usesVarName_list [varname] item) False secondExprs
extractDeclaration_varname varname decl = []

getOriginalDeclaration :: [String] -> VarName Anno -> Program Anno -> Maybe(String)
getOriginalDeclaration originalLines varname program = case declSrc_list of
														[] -> Nothing
														_ -> Just (extractOriginalCode "" originalLines declSrc)
			where 
				declSrc_list = everything (++) (mkQ [] (extractDeclaration_varnameSrcSpan varname)) program
				declSrc = head declSrc_list


extractDeclaration_varnameSrcSpan :: VarName Anno -> Decl Anno -> [SrcSpan]
extractDeclaration_varnameSrcSpan varname (Decl _ src lst _) 	| firstHasVar || secondHasVar = [src]
														| otherwise = []
			where
				firstExprs = map (\(expr, _, _) -> expr) lst
				secondExprs = map (\(_, expr, _) -> expr) lst

				firstHasVar = foldl (\accum item -> accum || usesVarName_list [varname] item) False firstExprs
				secondHasVar = foldl (\accum item -> accum || usesVarName_list [varname] item) False secondExprs
extractDeclaration_varnameSrcSpan varname decl = []


anyChildGenerated :: Fortran Anno -> Bool
anyChildGenerated ast = everything (||) (mkQ False isGenerated) ast

isGenerated :: Fortran Anno -> Bool
isGenerated codeSeg = f /= "<unknown>" || (lineStart == -1 && lineEnd == -1) || f == "generated"
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
						localReductionArray = generateArrayVar (generateLocalReductionArray (head (extractVarNames expr1))) [(generateVar redIter)]
						localReductionVar = generateVar (generateLocalReductionVar (head (extractVarNames expr1)))
generateWorkGroupReduction_assgs reductionVars redIter codeSeg = []

generateFinalHostReduction :: [VarName Anno] -> VarName Anno -> Fortran Anno -> Fortran Anno
generateFinalHostReduction reductionVars redIter codeSeg  = resultantCode
					where
						assignments = everything (++) (mkQ [] (generateFinalHostReduction_assgs reductionVars redIter)) codeSeg
						resultantCode = foldl1 (\accum item -> appendFortran_recursive item accum) assignments

generateFinalHostReduction_assgs :: [VarName Anno] -> VarName Anno -> Fortran Anno -> [Fortran Anno]
generateFinalHostReduction_assgs reductionVars redIter (Assg _ _ expr1 expr2) 	| isReductionExpr = resultantAssg
																				| otherwise = []
					where 
						isReductionExpr = usesVarName_list reductionVars expr1
						resultantAssg = case extractPrimaryReductionOp expr1 expr2 of
											Just op -> [Assg nullAnno nullSrcSpan finalReductionVar (Bin nullAnno nullSrcSpan op finalReductionVar finalReductionArray)]
											Nothing -> case extractPrimaryReductionFunction expr1 expr2 of
														"" -> []
														funcName -> [Assg nullAnno nullSrcSpan finalReductionVar (Var nullAnno nullSrcSpan [(VarName nullAnno funcName, [finalReductionVar, finalReductionArray])])]
						finalReductionArray = generateArrayVar (generateGlobalReductionArray (head (extractVarNames expr1))) [(generateVar redIter)]
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
				iterator,
				generateLoopStartAddition var start] 
generateLoopInitialisers ((var, start, end, step):[]) iterator (Just offset) 
			= 	[generateRangeExpr var start end,
				Assg nullAnno nullSrcSpan 
				(generateRelVar var)
				(offset),
				generateLoopStartAddition var start]

generateLoopInitialisers ((var, start, end, step):xs) iterator Nothing 
			= 	[generateRangeExpr var start end,
				Assg nullAnno nullSrcSpan 
				(generateRelVar var)
				(Bin nullAnno nullSrcSpan (Div nullAnno)  iterator multipliedExprs),
				generateLoopStartAddition var start]
				++
				generateLoopInitialisers xs iterator (Just nextOffset)
					where
						nextOffset = generateSubtractionExpr_list ([iterator] ++ [generateProductExpr_list ([generateRelVar var] ++ followingRangeExprs)])
						followingRangeExprs = map (\(v,_,_,_) -> generateRangeVar v) xs
						multipliedExprs = generateProductExpr_list followingRangeExprs 
generateLoopInitialisers ((var, start, end, step):xs) iterator (Just offset) 
			= 	[generateRangeExpr var start end,
				Assg nullAnno nullSrcSpan (generateRelVar var)
					(Bin nullAnno nullSrcSpan (Div nullAnno) 
						offset
						multipliedExprs),
				generateLoopStartAddition var start]
				++
				generateLoopInitialisers xs iterator (Just nextOffset)
					where
						nextOffset = generateSubtractionExpr_list ([offset] ++ [generateProductExpr_list ([generateRelVar var] ++ followingRangeExprs)])
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