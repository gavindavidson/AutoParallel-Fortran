module CodeEmitter 						(emit)

where

--	Code in this file handles the final emission of code. The function 'emit' is called against an AST that has been transformed and has had kernels fused.
--	Trys to make as much use as it can of code that is the same as it was in the original source file. Otherwise, it uses mostly simple functions to generate
--	code segments. Things get more complex when generating reduction kernels as a number of changes to the original source are needed to take full advantage
--	of parallel hardware.

--	NAMING CONVENTIONS:
--	-	"synthesise.." functions tend to take an AST and translate it into a string of fortran code
--	-	"generate.." functions tend to make new AST nodes

import Control.Monad
import Data.Generics 					(Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List
import System.IO
import System.Process
import Data.Maybe
import qualified Data.Map as DMap 

import LanguageFortranTools
import SubroutineTable 					(ArgumentTranslation, ArgumentTranslationSubroutines, emptyArgumentTranslation, getSubroutineArgumentTranslation, translateArguments,
										extractSubroutines, extractProgUnitName)

type KernelArgsIndexMap = DMap.Map (VarName Anno) Int

emit :: String -> [String] -> Bool -> [(Program Anno, String)] -> [(Program Anno, String)] -> ArgumentTranslationSubroutines -> (Program Anno, String) -> [VarName Anno] -> [VarName Anno] -> IO [()]
emit specified cppDFlags fixedForm programs_verboseArgs programs_optimisedBuffers argTranslations (mainAst, mainFilename) initWrites tearDownReads = do

				kernels_code <- mapM (emitKernels cppDFlags fixedForm) programs_verboseArgs
				let allKernels = foldl (++) [] kernels_code
				let kernelNames = map (snd) allKernels

				let originalFilenames = map (\x -> getModuleName (snd x)) programs_verboseArgs
				let superkernelName = map (toLower) ((foldl1 (\accum item -> accum ++ "_" ++ item) originalFilenames) ++ "_superkernel")
				let moduleName = "module_" ++ superkernelName
				let moduleFilename = specified ++ "/" ++ moduleName ++ ".f95"
				let newMainFilename = specified ++ "/" ++ (hostModuleName (getModuleName mainFilename)) ++ ".f95"
				let initModuleFilename = specified ++ "/" ++ (initModuleName moduleName) ++ ".f95"

				-- let initTearDownFilename = specified ++ "/" ++ initTearDownModuleName ++ ".f95"
				let (superKernel_module, allKernelArgsMap) = synthesiseSuperKernelModule moduleName superkernelName programs_verboseArgs allKernels
				let initModule = synthesiseInitModule moduleName superkernelName programs_verboseArgs allKernelArgsMap allKernels

				host_code <- mapM (produceCode_prog allKernelArgsMap argTranslations cppDFlags fixedForm moduleName superkernelName) programs_optimisedBuffers
				main_code <- produceCode_prog allKernelArgsMap argTranslations cppDFlags fixedForm moduleName superkernelName (mainAst, mainFilename)
				-- let initAndTearDownCode = synthesiseInitAndTearDownModule allKernelArgsMap mainAst initWrites tearDownReads
				let host_programs = zip host_code (map (\x -> specified ++ "/" ++ x ++ "_host.f95") originalFilenames)

				writeFile moduleFilename (if fixedForm then fixedFormFormat superKernel_module else superKernel_module)
				writeFile newMainFilename (if fixedForm then fixedFormFormat main_code else main_code)
				writeFile initModuleFilename (if fixedForm then fixedFormFormat initModule else initModule)
				-- writeFile initTearDownFilename (if fixedForm then fixedFormFormat initAndTearDownCode else initAndTearDownCode)
				mapM (\(code, filename) -> writeFile filename (if fixedForm then fixedFormFormat code else code)) host_programs

fixedFormFormat :: String -> String
fixedFormFormat inputStr = foldl (\accum item -> accum ++ "\n" ++ (fixedFormFormat_line item)) "" allLines
		where
			allLines = lines inputStr

fixedFormFormat_line :: String -> String
fixedFormFormat_line "" = ""
fixedFormFormat_line inputLine 	|	fixedFormFormat_isComment inputLine = inputLine
								|	otherwise =  addedLeadingWhiteSpace ++ thisLine
								++ if nextLineExists then lineCont ++ "\n" ++ (if nextLineNotContinue then nextLine else "") else ""
		where
			thisLine = (take lineLength_contAndWhiteSpace (inputLine))
			nextLine = (fixedFormFormat_line (drop lineLength_contAndWhiteSpace (inputLine)))

			whiteSpaceCount = fixedFormFormat_leadingWhiteSpaceCount inputLine
			addedLeadingWhiteSpace = foldl (\accum item -> accum ++ " ") "" [whiteSpaceCount + 1 .. desiredLeadingWhiteSpace]
			lineCont = " &"
			desiredLeadingWhiteSpace = 6
			desiredLineLength = 72
			lineLength_contAndWhiteSpace = (desiredLineLength - (length lineCont)) - (max 0 (desiredLeadingWhiteSpace - whiteSpaceCount))

			nextLineNotContinue = (not $ fixedFormFormat_containsOnlyContinuation nextLine)
			nextLineExists = nextLine /= ""

fixedFormFormat_containsOnlyContinuation :: String -> Bool
fixedFormFormat_containsOnlyContinuation (char:str) 	|	char == '&' = fixedFormFormat_containsOnlyContinuation str
										|	isSpace char = fixedFormFormat_containsOnlyContinuation str
										|	otherwise = False
fixedFormFormat_containsOnlyContinuation [] = True

fixedFormFormat_isComment :: String -> Bool
fixedFormFormat_isComment (char:str) 	|	char == '!' = True
										|	isSpace char = fixedFormFormat_containsOnlyContinuation str
										|	otherwise = False
fixedFormFormat_isComment [] = True

fixedFormFormat_leadingWhiteSpaceCount :: String -> Int
fixedFormFormat_leadingWhiteSpaceCount (char:str) 	|	isSpace char = 1 + fixedFormFormat_leadingWhiteSpaceCount str
													|	otherwise = 0

generateArgList :: [VarName Anno] -> ArgList Anno
generateArgList [] = ArgList nullAnno (NullExpr nullAnno nullSrcSpan)
generateArgList vars = ArgList nullAnno (generateESeq vars)

generateESeq :: [VarName Anno] -> Expr Anno
generateESeq (var:[]) = generateVar var
generateESeq (var:vars) = ESeq nullAnno nullSrcSpan (generateESeq vars) (generateVar var)

extractBufferReadClusters_fortran :: Fortran Anno -> [(Fortran Anno, Fortran Anno, [VarName Anno])]
extractBufferReadClusters_fortran codeSeg = case codeSeg of
												(FSeq _ _ (OpenCLBufferRead _ _ _) fortran2) -> [(codeSeg, fortranFollowingCluster, clusterVarNames)]
													where
														(fortranFollowingCluster, clusterVarNames) = extractBufferReadClusters_fortranAfterReadCluster fortran2
												_ -> []

extractBufferReadClusters_fortranAfterReadCluster :: Fortran Anno -> (Fortran Anno, [VarName Anno])
extractBufferReadClusters_fortranAfterReadCluster (FSeq _ _ (OpenCLBufferRead _ _ var) fortran2) = (recurseFortran, var:recurseVarNames)
		where
			(recurseFortran, recurseVarNames) = extractBufferReadClusters_fortranAfterReadCluster fortran2
extractBufferReadClusters_fortranAfterReadCluster codeSeg = (codeSeg, [])

emitKernels :: [String] -> Bool -> (Program Anno, String) -> IO [(String, String)]
emitKernels cppDFlags fixedForm (ast, filename) = do
				originalLines <- readOriginalFileLines cppDFlags fixedForm filename
				let originalListing = case originalLines of
										[]	-> ""
										_ -> foldl (\accum item -> accum ++ "\n" ++ item) (head originalLines) (tail originalLines)
				let kernels_code = everything (++) (mkQ [] (synthesiseKernels originalLines (ast, filename))) ast
				let kernels_renamed = map (\(code, kernelname) -> (code, kernelname)) kernels_code
				return kernels_renamed

synthesiseInitModule :: String -> String ->  [(Program Anno, String)] -> KernelArgsIndexMap -> [(String, String)] -> String
-- synthesiseInitModule :: String -> String -> (Program Anno, String) -> KernelArgsIndexMap -> [(String, String)] -> String
synthesiseInitModule moduleName superKernelName programs allKernelArgsMap kernels = 	initModuleHeader 
-- synthesiseInitModule moduleName superKernelName (ast, filename) allKernelArgsMap kernels = 	initModuleHeader 
										++ 	stateDefinitions 
										++	"\ncontains\n\n"
										++	oneTab ++ initSubroutineHeader
											++	twoTab ++ "use oclWrapper\n"
											++	usesString
											++	kernelInitialisationStrs
											++	declarationsStr
											++	bufferDeclarationStatements
											++	sizeDeclarations_str ++ "\n"

											++	produceCode_fortran ([], "") twoTab [] oclInitCall
											++	"\n" ++ shapeStatements_str
											++	"\n" ++ makeBufferStatementStr
											++	"\n" ++ oclSetArgStatements_str
											++	"\n" ++ storeBufferStatements_str ++ "\n"
				 							
										++	"\n" ++ oneTab ++ initSubroutineFooter
										++ 	initModuleFooter
				where
					kernelNames = map (snd) kernels
					initModuleHeader = "module " ++ (initModuleName moduleName) ++ "\n\n"
					initModuleFooter = "\nend module " ++ (initModuleName moduleName)

					initSubroutineHeader = "subroutine " ++ (initModuleName superKernelName) ++ "()\n\n"
					initSubroutineFooter = "end subroutine " ++ (initModuleName superKernelName)

					stateNames = map (generateStateName) kernelNames
					stateDefinitions = synthesiseStateDefinitions (zip kernelNames stateNames) 0

					kernelInitialisationStrs = 	twoTab ++ "character(len=*), parameter :: srcstr = \"" ++ moduleName ++".cl\"\n"
     										++	twoTab ++ "character(len=*), parameter :: kstr   = \"" ++ superKernelName ++ "\"\n"

					oclInitCall = Call nullAnno nullSrcSpan (generateVar (VarName nullAnno "oclInit")) (generateArgList [VarName nullAnno "kstr", VarName nullAnno "srcstr"])

					kernelArgs = DMap.keys allKernelArgsMap

					programAsts = map (fst) programs
					kernelAstLists = map (extractKernels) programAsts
					kernelAsts = foldl (++) [] (map (\(k_asts, p_ast) -> map (\a -> (a, p_ast)) k_asts) (zip kernelAstLists programAsts))

					extractedUses = foldl (\accum item -> listConcatUnique accum (everything (++) (mkQ [] getUses) item)) [] programAsts
					usesString = foldl (\accum item -> accum ++ synthesisUses twoTab item) "" extractedUses 

					kernelDeclarations = map (\(kernel_ast, prog_ast) -> generateKernelDeclarations prog_ast kernel_ast) kernelAsts
					(readDecls, writtenDecls, generalDecls) =  foldl (\(accum_r, accum_w, accum_g) (r, w, g) -> (accum_r ++ r, accum_w ++ w, accum_g ++ g)) ([],[],[]) kernelDeclarations
					declarations = foldl (collectDecls) [] (readDecls ++ writtenDecls ++ generalDecls ++ [statePtrDecl])
					declarations_noIntent = map (removeIntentFromDecl) declarations
					declarationsStr = foldl (\accum item -> accum ++ synthesiseDecl twoTab item) "" declarations_noIntent

					makeBuffers = map (synthesiseBufferMake twoTab) declarations
					makeBufferStatementStr = foldl (\accum item -> accum ++ item ++ "\n") "" makeBuffers

					-- bufferDeclarationStatements = foldl (\accum var -> accum ++ twoTab ++ "integer(8) :: " ++ (varNameStr (varBufVarName var)) ++ "\n") "" kernelArgs
					-- synthesiseBufferDeclaration
					bufferDeclarationStatements = foldl (\accum decl -> accum ++ (synthesiseBufferDeclaration twoTab decl) ++ "\n") "" declarations_noIntent

					(sizeDeclarations, shapeStatements) = generateSizeStatements_decls twoTab declarations
					sizeDeclarations_noIntent = map (removeIntentFromDecl) sizeDeclarations
					sizeDeclarations_str = foldl (\accum item -> accum ++ (synthesiseDecl twoTab item)) "" sizeDeclarations_noIntent
					shapeStatements_str = foldl (\accum item -> accum ++ (produceCode_fortran ([], "") twoTab [] item)) "" shapeStatements

					(_, storeBufferStatements_str) = synthesiseBufferStores twoTab allKernelArgsMap emptyArgumentTranslation kernelArgs

					oclSetArgStatements_str = foldl (\accum item -> accum ++ (synthesiseSetOclArg twoTab allKernelArgsMap item) ++ "\n") "" declarations_noIntent

					oneTab = tabInc
					twoTab = oneTab ++ tabInc

synthesiseSuperKernelModule :: String -> String -> [(Program Anno, String)] -> [(String, String)] -> (String, KernelArgsIndexMap)
synthesiseSuperKernelModule moduleName superKernelName programs kernels =  (kernelModuleHeader
																			++ useStatements
																			++ contains
																			++ stateDefinitions
																			++ "\n"
																			++ (foldl (++) "" kernelCode) 
																			++ superKernelCode 
																			++ kernelModuleFooter,
															allKernelArgsMap)
				where
					kernelCode = map (fst) kernels
					kernelNames = map (snd) kernels
					
					kernelModuleHeader = "module " ++ moduleName ++ "\n\n"
					useStatements = "use " ++ initModuleName moduleName ++ "\n"
					contains = "\n" ++ tabInc ++ "contains\n\n"
					kernelModuleFooter = "end module " ++ moduleName

					(superKernelCode, allKernelArgsMap) = synthesiseSuperKernel outputTab superKernelName programs kernels

					stateNames = map (generateStateName) kernelNames
					stateDefinitions = synthesiseStateDefinitions (zip kernelNames stateNames) 0

generateStateName :: String -> String
generateStateName kernelName = "ST_" ++ (map (toUpper) kernelName)

synthesiseStateDefinitions :: [(String, String)] -> Int -> String
synthesiseStateDefinitions [] currentVal =  ""
synthesiseStateDefinitions ((kernel, state):xs) currentVal =  "integer, parameter :: " ++ state ++ " = " ++ show currentVal ++ " !  " ++ kernel ++ "\n" ++ (synthesiseStateDefinitions xs (currentVal+1))

synthesiseSuperKernel :: String -> String -> [(Program Anno, String)] -> [(String, String)] -> (String, KernelArgsIndexMap)
synthesiseSuperKernel tabs name programs [] = ("", DMap.empty)
synthesiseSuperKernel tabs name programs kernels = if allKernelArgs == [] then error "synthesiseSuperKernel" else (superKernel, allKernelArgsMap)
													-- error ("kernelDeclarations: " ++ (show kernelDeclarations)
													-- 	++ "\n\nreadDecls: " ++ (show readDecls)
													-- 	++ "\n\nwrittenDecls: " ++ (show writtenDecls)
													-- 	++ "\n\ngeneralDecls: " ++ (show generalDecls)
													-- 	)
				where
					programAsts = map (fst) programs
					kernelAstLists = map (extractKernels) programAsts

					kernelAsts = foldl (++) [] (map (\(k_asts, p_ast) -> map (\a -> (a, p_ast)) k_asts) (zip kernelAstLists programAsts))
					kernelArgs = (map (\(x, _) -> extractKernelArguments x) kernelAsts)
					allKernelArgs = (listRemoveDuplications (foldl (++) [] kernelArgs)) ++ [statePtrVarName]

					allKernelArgsMap = foldl (\dmap (arg, index) -> DMap.insert arg index dmap) DMap.empty (zip allKernelArgs ([1..(length allKernelArgs)]))

					kernelDeclarations = map (\(kernel_ast, prog_ast) -> generateKernelDeclarations prog_ast kernel_ast) kernelAsts
					(readDecls, writtenDecls, generalDecls) =  foldl (\(accum_r, accum_w, accum_g) (r, w, g) -> (accum_r ++ r, accum_w ++ w, accum_g ++ g)) ([],[],[]) kernelDeclarations
					-- (readDecls, writtenDecls, generalDecls) = foldl (collectSuperKernelDecls) ([],[],[]) kernelDeclarations

					-- declarations = listRemoveDuplications (readDecls ++ writtenDecls ++ generalDecls)
					declarations = foldl (collectDecls) [] (readDecls ++ writtenDecls ++ generalDecls)
					declarationsStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" declarations

					-- statePointerDeclStr = tabs ++ "integer, dimension(256) :: " ++ (varNameStr stateVarName) ++ "_ptr\n"
					stateVarDeclStr = synthesiseDecl tabs stateVarDecl
					statePointerDeclStr = synthesiseDecl tabs statePtrDecl
					stateAssignment = tabs ++ (varNameStr stateVarName) ++ " = " ++ (varNameStr stateVarName) ++ "_ptr(1)\n"

					superKernel_header = "subroutine " ++ name ++ "(" ++ (foldl (\accum item -> accum ++ "," ++ (varNameStr item)) (varNameStr $ head allKernelArgs) (tail allKernelArgs)) ++ ")\n"
					superKernel_footer = "end subroutine " ++ name ++ "\n"
					superKernel_body = "! SUPERKERNEL BODY\n" ++ selectCase

					kernelNames = map (snd) kernels
					stateNames = map (generateStateName) kernelNames

					caseAlternatives = foldl (\accum (state, name, args) -> accum ++ synthesiseKernelCaseAlternative (tabs ++ outputTab) state name args) "" (zip3 stateNames kernelNames kernelArgs)
					selectCase = outputTab ++ "select case(" ++ (varNameStr stateVarName) ++ ")\n" ++ caseAlternatives ++ outputTab ++ "end select\n"

					superKernel = superKernel_header ++ declarationsStr ++ "\n" ++ stateVarDeclStr ++ statePointerDeclStr ++ stateAssignment ++ superKernel_body ++ superKernel_footer
				
collectDecls :: [Decl Anno] -> Decl Anno -> [Decl Anno]
collectDecls previousDecls currentDecl = mergeDeclWithPrevious_recurse previousDecls currentDecl

mergeDeclWithPrevious_recurse :: [Decl Anno] -> Decl Anno -> [Decl Anno]
mergeDeclWithPrevious_recurse (listDecl:decls) currentDecl 	|	matchingVarNames && currentDeclIntentAttrs == listDeclIntentAttrs = currentDecl:decls
															|	matchingVarNames && currentDeclIntentAttrs /= listDeclIntentAttrs = adaptedIntentDecl:decls
															|	otherwise = listDecl:(mergeDeclWithPrevious_recurse decls currentDecl)
			where
				listDeclName = extractAssigneeFromDecl listDecl
				currentDeclName = extractAssigneeFromDecl currentDecl
				matchingVarNames = listDeclName == currentDeclName

				listDeclIntentAttrs = everything (++) (mkQ [] extractintentAttrs) listDecl
				currentDeclIntentAttrs = everything (++) (mkQ [] extractintentAttrs) currentDecl

				adaptedIntentDecl = applyIntent (InOut nullAnno) listDecl
mergeDeclWithPrevious_recurse [] currentDecl = [currentDecl]

-- collectSuperKernelDecls :: ([Decl Anno], [Decl Anno], [Decl Anno]) -> ([Decl Anno], [Decl Anno], [Decl Anno]) -> ([Decl Anno], [Decl Anno], [Decl Anno]) 
-- collectSuperKernelDecls (accumReads, accumWrites, accumGen) (itemReads, itemWrites, itemGen) = (newReads, newWrites, newGen')
-- 				where
-- 					(newReads, newGen) = foldl (collectSuperKernelDecls_singleType (writtenVars, genVars)) (accumReads, accumGen) itemReads
-- 					(newWrites, newGen') = foldl (collectSuperKernelDecls_singleType (writtenVars, newGenVars)) (accumReads, newGen) itemReads

-- 					readsVars = map (extractAssigneeFromDecl) accumReads
-- 					writtenVars = map (extractAssigneeFromDecl) accumWrites
-- 					genVars = map (extractAssigneeFromDecl) accumGen
-- 					newGenVars = map (extractAssigneeFromDecl) newGen

-- collectSuperKernelDecls_singleType :: ([VarName Anno], [VarName Anno]) -> ([Decl Anno], [Decl Anno]) -> Decl Anno -> ([Decl Anno], [Decl Anno])
-- collectSuperKernelDecls_singleType (opTypeVars, genVars) (typeDecls, genDecls) item 	|	itemAppears_gen = (removeDeclWithVarName typeDecls itemVar, genDecls)
-- 																						|	itemAppears_opType = (removeDeclWithVarName typeDecls itemVar, genDecls ++ [applyIntent (InOut nullAnno) item])
-- 																						|	otherwise = (typeDecls ++ [item], genDecls)
-- 				where
-- 					itemVar = extractAssigneeFromDecl item
-- 					itemAppears_opType = elem itemVar opTypeVars
-- 					itemAppears_gen = elem itemVar genVars

removeDeclWithVarName :: [Decl Anno] -> VarName Anno -> [Decl Anno]
removeDeclWithVarName decls var = filter (\x -> (extractAssigneeFromDecl x) /= var) decls

synthesiseKernelCaseAlternative :: String -> String -> String -> [VarName Anno] -> String
synthesiseKernelCaseAlternative tabs state kernelName [] = error "synthesiseKernelCaseAlternative"
synthesiseKernelCaseAlternative tabs state kernelName args =  tabs ++ "case (" ++ state ++ ")\n" ++ tabs ++ outputTab ++ kernelName ++ "(" ++ argsString ++ ")" ++ "\n" 
				where
					argsString = foldl (\accum item -> accum ++ "," ++ (varNameStr item)) (varNameStr $ head args) (tail args)

extractKernelArguments :: Fortran Anno -> [VarName Anno]
extractKernelArguments (OpenCLMap _ _ r w _ _) = listRemoveDuplications (r ++ w)
extractKernelArguments (OpenCLReduce _ _ r w _ rv _) = listRemoveDuplications ((listSubtract (r ++ w) rvVarNames) ++ (map (\x -> generateGlobalReductionArray (fst x)) rv))
				where
					rvVarNames = map (fst) rv
extractKernelArguments _ = []

defaultFilename :: [String] -> String
defaultFilename (x:[]) = "par_" ++ x
defaultFilename (x:xs) = x ++ "/" ++ defaultFilename xs

generateOriginalFileName :: [String] -> String
generateOriginalFileName (x:[]) = "original_" ++ x
generateOriginalFileName (x:xs) = x ++ "/" ++ generateOriginalFileName xs

--	This function produces a list of strings where each element is a line of the original source. This
--	list is used heavily in this module.
readOriginalFileLines :: [String] -> Bool -> String -> IO ([String])
readOriginalFileLines cppDFlags fixedForm filename = do
				content <- cpp cppDFlags fixedForm filename
				let contentLines = lines content
				return contentLines

synthesiseKernels :: [String] -> (Program Anno, String) -> Fortran Anno -> [(String, String)]
synthesiseKernels originalLines prog codeSeg = case codeSeg of
				OpenCLMap _ src _ w _ _ -> [(synthesiseOpenCLMap "" originalLines prog codeSeg, generateKernelName "map" src w)]
				OpenCLReduce _ src _ _ _ rv _ ->  [(synthesiseOpenCLReduce "" originalLines prog codeSeg, generateKernelName "reduce" src (map (\(v, e) -> v) rv))]
				_ -> []

generateKernelDeclarations :: Program Anno -> Fortran Anno -> ([Decl Anno], [Decl Anno], [Decl Anno])
generateKernelDeclarations prog (OpenCLMap _ _ r w _ _) = (readDecls, writtenDecls, generalDecls)
				where
					readArgs = listSubtract r w
					writtenArgs = listSubtract w r
					generalArgs = listIntersection w r
					
					readDecls = map (\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (In nullAnno) prog)) readArgs
					writtenDecls = map (\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (Out nullAnno) prog)) writtenArgs
					generalDecls = map (\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (InOut nullAnno) prog)) generalArgs
generateKernelDeclarations prog (OpenCLReduce _ _ r w _ rv _) = (readDecls, writtenDecls, generalDecls_withReductions)
				where
					reductionVarNames = map (fst) rv

					readArgs = listSubtract (listSubtract r w) reductionVarNames
					writtenArgs = listSubtract (listSubtract w r) reductionVarNames
					generalArgs = listSubtract (listIntersection w r) reductionVarNames

					readDecls = map (\x -> fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (In nullAnno) prog)) readArgs
					writtenDecls = map (\x -> fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (Out nullAnno) prog)) writtenArgs
					generalDecls = map (\x -> fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (InOut nullAnno) prog)) generalArgs

					globalReductionDecls = (map (\x -> declareGlobalReductionArray x (nunitsVar) (prog)) reductionVarNames)

					generalDecls_withReductions = generalDecls ++ globalReductionDecls
generateKernelDeclarations prog (OpenCLBufferRead _ _ varName) = (readDecls, [], [])
				where
					readDecls = [(\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (In nullAnno) prog)) varName]
generateKernelDeclarations prog (OpenCLBufferWrite _ _ varName) = ([], writtenDecls, [])
				where
					writtenDecls = [(\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (Out nullAnno) prog)) varName]


produceCode_prog :: KernelArgsIndexMap -> ArgumentTranslationSubroutines -> [String] -> Bool -> String -> String -> (Program Anno, String) -> IO(String)
produceCode_prog allKernelArgsMap argTranslation cppDFlags fixedForm kernelModuleName superKernelName (prog, filename) = do
					originalLines <- readOriginalFileLines cppDFlags fixedForm filename
					let result = foldl (\accum item -> accum ++ produceCode_progUnit allKernelArgsMap emptyArgumentTranslation (prog, filename) kernelModuleName superKernelName originalLines item) "" prog
					return result

produceCode_progUnit :: KernelArgsIndexMap -> ArgumentTranslationSubroutines -> (Program Anno, String) -> String -> String -> [String] -> ProgUnit Anno -> String
produceCode_progUnit allKernelArgsMap argTranslationSubroutines progWithFilename kernelModuleName superKernelName originalLines (Main _ src _ _ block progUnits) =	
																																nonGeneratedHeaderCode
																															++ 	nonGeneratedBlockCode_indent ++ "use oclWrapper\n" 
																															++ 	nonGeneratedBlockCode_indent ++ "use " ++ (initModuleName kernelModuleName) ++ "\n" 
																														 	-- ++ 	kernelInitialisationStrs
																														 	-- ++	produceCode_fortran progWithFilename nonGeneratedBlockCode_indent originalLines oclInitCall
																															-- ++ 	everything (++) (mkQ "" (produceCodeBlock allKernelArgsMap emptyArgumentTranslation progWithFilename nonGeneratedBlockCode_indent originalLines)) blockWithInit
																															++ 	everything (++) (mkQ "" (produceCodeBlock allKernelArgsMap emptyArgumentTranslation progWithFilename nonGeneratedBlockCode_indent originalLines maybeOclInitCall)) block
																															++	containedProgUnitCode
																															++	nonGeneratedFooterCode
		where
			(Block blockAnno useBlock implicit blockSrc blockDecl blockFortran) = block
			-- blockSrc = srcSpan block
			((SrcLoc _ block_ls _), _) = blockSrc
			(nonGeneratedHeaderSrc, nonGeneratedFooterSrc) = getSrcSpanNonIntersection src blockSrc

			((SrcLoc _ nonGeneratedHeader_ls _), (SrcLoc _ nonGeneratedHeader_le _)) = nonGeneratedHeaderSrc
			nonGeneratedHeaderCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedHeader_ls..nonGeneratedHeader_le-1]
			
			((SrcLoc _ nonGeneratedFooter_ls _), (SrcLoc _ nonGeneratedFooter_le _)) = nonGeneratedFooterSrc
			nonGeneratedFooterCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedFooter_ls..nonGeneratedFooter_le-1]

			containedProgUnitCode = foldl (\accum item -> accum ++ (produceCode_progUnit allKernelArgsMap argTranslationSubroutines progWithFilename kernelModuleName superKernelName originalLines item)) "" progUnits
			nonGeneratedBlockCode_indent = extractIndent (originalLines!!(block_ls-1))

			maybeOclInitCall = Just (Call nullAnno nullSrcSpan (generateVar (VarName nullAnno ((initModuleName superKernelName)))) (ArgList nullAnno (NullExpr nullAnno nullSrcSpan)))
			-- blockFortranWithInit = FSeq nullAnno (srcSpan blockFortran) oclInitCall blockFortran
			-- blockWithInit = Block blockAnno useBlock implicit blockSrc blockDecl blockFortranWithInit

produceCode_progUnit allKernelArgsMap argTranslationSubroutines progWithFilename kernelModuleName superKernelName originalLines (Module _ src _ _ _ _ progUnits) = 	nonGeneratedHeaderCode 
																																++ 	containedProgUnitCode
																																++	nonGeneratedFooterCode
		where
			firstProgUnitSrc = srcSpan (head progUnits)
			lastProgUnitSrc = srcSpan (last progUnits)
			
			(nonGeneratedHeaderSrc, _) = getSrcSpanNonIntersection src firstProgUnitSrc
			(_, nonGeneratedFooterSrc) = getSrcSpanNonIntersection src lastProgUnitSrc
			((SrcLoc _ nonGeneratedHeader_ls _), (SrcLoc _ nonGeneratedHeader_le _)) = nonGeneratedHeaderSrc
			nonGeneratedHeaderCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedHeader_ls..nonGeneratedHeader_le-1]
			((SrcLoc _ nonGeneratedFooter_ls _), (SrcLoc _ nonGeneratedFooter_le _)) = nonGeneratedFooterSrc
			nonGeneratedFooterCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedFooter_ls..nonGeneratedFooter_le-1]

			containedProgUnitCode = foldl (\accum item -> accum ++ (produceCode_progUnit allKernelArgsMap argTranslationSubroutines progWithFilename kernelModuleName superKernelName originalLines item)) "" progUnits
produceCode_progUnit allKernelArgsMap argTranslationSubroutines progWithFilename kernelModuleName superKernelName originalLines (Sub _ src _ (SubName _ subroutineName) _ block) =  nonGeneratedHeaderCode 
												++ 	nonGeneratedBlockCode_indent ++ "use " ++ (initModuleName kernelModuleName) ++ "\n" 
												++ 	nonGeneratedBlockCode_indent ++ "use oclWrapper\n" 
												++	nonGeneratedBlockCode_indent ++ "real (kind=4) :: exectime\n"
												++	global_reductionArraysDeclStr
												-- ++ 	everything (++) (mkQ "" (produceCodeBlock allKernelArgsMap argTranslation progWithFilename nonGeneratedBlockCode_indent originalLines)) blockWithReductionArrayDecls
												++ 	everything (++) (mkQ "" (produceCodeBlock allKernelArgsMap argTranslation progWithFilename nonGeneratedBlockCode_indent originalLines Nothing)) block
												-- ++ 	"! Begin progunit footer\n"
												++ 	nonGeneratedFooterCode	
												-- ++ 	"! End progunit footer\n"
		where
			firstFortranSrc = head (everything (++) (mkQ [] (getFirstFortranSrc)) block)
			blockSrc = srcSpan block
			(nonGeneratedHeaderSrc, nonGeneratedFooterSrc) = getSrcSpanNonIntersection src blockSrc

			((SrcLoc _ nonGeneratedHeader_ls _), (SrcLoc _ nonGeneratedHeader_le _)) = nonGeneratedHeaderSrc
			nonGeneratedHeaderCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedHeader_ls..nonGeneratedHeader_le-1]

			((SrcLoc _ nonGeneratedFooter_ls _), (SrcLoc _ nonGeneratedFooter_le _)) = nonGeneratedFooterSrc
			nonGeneratedFooterCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedFooter_ls..nonGeneratedFooter_le-1]

			((SrcLoc _ block_ls _), (SrcLoc _ _ _)) = blockSrc
			((SrcLoc _ fortran_ls _), (SrcLoc _ _ _)) = firstFortranSrc
			nonGeneratedBlockCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [block_ls..fortran_ls-1]

			nonGeneratedBlockCode_indent = extractIndent (originalLines!!(fortran_ls-1))

			argTranslation = getSubroutineArgumentTranslation argTranslationSubroutines subroutineName

			reduceKernels = extractOpenCLReduces block
			reductionVarNames = foldl (\accum item -> listConcatUnique accum (extractReductionVarNames item)) [] reduceKernels

			global_reductionArraysDecls = map (\x -> declareGlobalReductionArray x (nunitsVar) (fst progWithFilename)) reductionVarNames
			global_reductionArraysDeclsWithoutIntent = map (removeIntentFromDecl) global_reductionArraysDecls
			global_reductionArraysDeclStr = synthesiseDecls nonGeneratedBlockCode_indent global_reductionArraysDeclsWithoutIntent

			-- blockWithReductionArrayDecls = gmapT (mkT (insertDecls global_reductionArraysDecls)) block

produceCode_progUnit allKernelArgsMap argTranslationSubroutines progWithFilename kernelModuleName superKernelName originalLines progunit = foldl (++) "" (gmapQ (mkQ "" (produceCode_progUnit allKernelArgsMap argTranslationSubroutines progWithFilename kernelModuleName superKernelName originalLines)) progunit)

synthesiseSizeStatements :: String -> [VarName Anno] -> Program Anno -> (String, String)
synthesiseSizeStatements tabs vars ast = (sizeDeclarations ++ scalarPointerDeclarationStrs, shapeStatements)
		where
			shapeStatements = foldl (\accum varname -> accum ++ tabs ++ (varNameStr (varSizeVarName varname)) ++ " = shape(" ++ (varNameStr varname) ++ ")\n") "" vars_onlyArrays
			sizeDeclarations = foldl (\accum (varname, rank) -> accum ++ tabs ++ "integer, dimension(" ++ (show rank) ++ ") :: " ++ (varNameStr (varSizeVarName varname)) ++ "\n") "" varsWithRanks_arrays
			scalarPointerDeclarationStrs = synthesiseDecls tabs scalarPointerDeclarations

			reduceKernels = extractOpenCLReduces ast
			reductionVarNames = foldl (\accum item -> accum ++ (extractReductionVarNames item)) [] reduceKernels
			global_reductionArraysDecls = map (\x -> declareGlobalReductionArray x (nunitsVar) ast) reductionVarNames
			global_reductionArrayNames = map (generateGlobalReductionArray) reductionVarNames

			allVars = (listSubtract vars global_reductionArrayNames) ++ global_reductionArrayNames -- Odd code means that allVars has varNames in the correct order

			decl_list = foldl (\accum item -> accum ++ extractDeclaration_varname item ast) [] vars
			dimensionRanks = map (getDeclRank) (decl_list ++ global_reductionArraysDecls)
			varsWithRanks = (statePtrVarName, 1):(zip allVars dimensionRanks)
			-- varsWithRanks_scalarPointer = map (\(var, rank) -> (var, max 1 rank)) varsWithRanks
			varsWithRanks_arrays = map (\(var, rank) -> if rank == 0 then (scalarPointerVarName var, 1) else (var, rank)) varsWithRanks

			vars_onlyArrays = map (\(var, rank) -> if rank == 0 then scalarPointerVarName var else var) varsWithRanks
			vars_onlyScalars = listSubtract allVars vars_onlyArrays
			scalarPointerDeclarations = map (\x -> declareScalarPointer x ast) vars_onlyScalars

generateSizeStatements_decls :: String -> [Decl Anno] -> ([Decl Anno], [Fortran Anno])
generateSizeStatements_decls tabs decls = (sizeDeclarations ++ scalarPointerDeclarations, shapeStatements)
		where 
			shapeStatements = foldl (\accum varname -> accum ++ [generateSizeStatement varname]) [] vars_onlyArrays
			sizeDeclarations = foldl (\accum (varname, rank) -> accum ++ [generateSizeDecl (varSizeVarName varname) rank]) [] varsWithRanks_arrays
			scalarPointerDeclarationStrs = synthesiseDecls tabs scalarPointerDeclarations

			allVars = map (extractAssigneeFromDecl) decls 

			dimensionRanks = map (getDeclRank) (decls)
			varsWithRanks = zip allVars dimensionRanks
			varsWithRanks_arrays = map (\(var, rank) -> if rank == 0 then (scalarPointerVarName var, 1) else (var, rank)) varsWithRanks

			vars_onlyArrays = map (\(var, rank) -> if rank == 0 then scalarPointerVarName var else var) varsWithRanks
			
			scalarDecls = filter (\x -> 0 == getDeclRank x) decls
			scalarPointerDeclarations = map (declareScalarPointer_decl) scalarDecls


generateSizeDecl :: VarName Anno -> Int -> Decl Anno
generateSizeDecl varname rank = Decl nullAnno nullSrcSpan [(generateVar varname, (NullExpr nullAnno nullSrcSpan), Nothing)] 
										(BaseType nullAnno (Integer nullAnno) [Dimension nullAnno [(NullExpr nullAnno nullSrcSpan, rankConst)]] (NullExpr nullAnno nullSrcSpan) (eight))
		where
			eight = generateIntConstant 8
			rankConst = generateIntConstant rank

-- Var p SrcSpan  [(VarName p, [Expr p])]
generateSizeStatement :: VarName Anno -> Fortran Anno
generateSizeStatement varname = Assg nullAnno nullSrcSpan assignee assignment
		where
			assignee = generateVar (varSizeVarName varname)
			assignment = Var nullAnno nullSrcSpan [(VarName nullAnno "shape", [generateVar varname])]

synthesiseSizeStatements_kernel :: String -> Program Anno -> (String, String)
synthesiseSizeStatements_kernel tabs ast = synthesiseSizeStatements tabs allBufferAccesses ast
		where
			kernels = extractKernels ast
			bufferReads = extractBufferReads ast
			bufferWrites = extractBufferWrites ast
			
			kernelArgs = listRemoveDuplications (foldl (\accum item -> accum ++ (extractKernelArguments item)) [] kernels) -- map (extractKernelArguments) kernels
			bufferReadVars = map (\(OpenCLBufferRead _ _ var) -> var) bufferReads
			bufferWrittenVars = map (\(OpenCLBufferWrite _ _ var) -> var) bufferWrites
		
			allBufferAccesses = listRemoveDuplications (kernelArgs ++ bufferWrittenVars ++ bufferReadVars)


synthesiseSetOclArg :: String -> KernelArgsIndexMap -> Decl Anno -> String
synthesiseSetOclArg tabs allKernelArgsMap (Decl anno src lst typ) = case baseType of
														Integer _ -> prefix ++ "Int" ++ suffix
														Real _ -> prefix ++ "Float" ++ suffix
														_ -> ""
		where
			assigneeName = extractAssigneeFromDecl (Decl anno src lst typ)
			kernelArgIndex_C = (DMap.findWithDefault (error "synthesiseSetOclArg: arg doesn't exist in KernelArgsIndexMap") assigneeName allKernelArgsMap) - 1

			baseType = extractBaseType typ

			prefix = tabs ++ "call oclSet"
			suffix = "ArrayArg(" ++ (show kernelArgIndex_C) ++ ", " ++ (varNameStr (varBufVarName assigneeName)) ++ ")"

synthesiseBufferDeclaration :: String -> Decl Anno -> String
synthesiseBufferDeclaration tabs decl = tabs ++ "integer(8) :: " ++ (varNameStr (varBufVarName assignee))
		where
			assignee = extractAssigneeFromDecl decl

synthesiseBufferStores :: String -> KernelArgsIndexMap -> ArgumentTranslation -> [VarName Anno] -> (String, String)
synthesiseBufferStores tabs allKernelArgsMap argTranslation vars = (declarationStatements, storeBufferStatements)
		where
			storeBufferStatements = foldl (\accum (var, index) -> accum ++ tabs ++ "call oclStoreBuffer(" ++ (show index) ++ ", " ++ (varNameStr (varBufVarName var)) ++ ")" ++  "\n") "" kernelBufferIndices
			declarationStatements = foldl (\accum var -> accum ++ tabs ++ "integer(8) :: " ++ (varNameStr (varBufVarName var)) ++ "\n") "" translatedVars

			translatedVars = translateArguments argTranslation vars
			kernelBufferIndices = zip vars (map (\x -> DMap.findWithDefault (-1) x allKernelArgsMap) translatedVars)

synthesiseBufferLoads :: String -> KernelArgsIndexMap -> ArgumentTranslation -> [VarName Anno] -> (String, String)
synthesiseBufferLoads tabs allKernelArgsMap argTranslation vars = (declarationStatements, loadBufferStatements)
		where
			loadBufferStatements = foldl (\accum (var, index) -> accum ++ tabs ++ "call oclLoadBuffer(" ++ (show index) ++ ", " ++ (varNameStr (varBufVarName var)) ++ ")" ++  "\n") "" kernelBufferIndices
			declarationStatements = foldl (\accum var -> accum ++ tabs ++ "integer(8) :: " ++ (varNameStr (varBufVarName var)) ++ "\n") "" translatedVars

			translatedVars = translateArguments argTranslation vars
			kernelBufferIndices = zip vars (map (\x -> DMap.findWithDefault (-1) x allKernelArgsMap) translatedVars)

synthesiseBufferLoads_kernel :: String -> KernelArgsIndexMap -> ArgumentTranslation -> Block Anno -> (String, String)
synthesiseBufferLoads_kernel tabs allKernelArgsMap argTranslation ast = synthesiseBufferLoads tabs allKernelArgsMap argTranslation allBufferAccesses
		where
			kernels = extractKernels ast
			bufferReads = extractBufferReads ast
			bufferWrites = extractBufferWrites ast
			
			kernelArgs = listRemoveDuplications (foldl (\accum item -> accum ++ (extractKernelArguments item)) [] kernels)
			bufferReadVars = map (\(OpenCLBufferRead _ _ var) -> var) bufferReads
			bufferWrittenVars = map (\(OpenCLBufferWrite _ _ var) -> var) bufferWrites

			allBufferAccesses = listRemoveDuplications ([statePtrVarName] ++ kernelArgs ++ bufferWrittenVars ++ bufferReadVars)

--	Function is used (along with "getFirstBlockSrc") by "produceCode_progUnit" to determine which lines of the original
--	source can be taken as is. It is used to determine where the first Fortran nodes of the AST appear in the source
--	because the Fortran nodes are the ones that have been transformed.
getFirstFortranSrc :: Block Anno -> [SrcSpan]
getFirstFortranSrc (Block _ _ _ _ _ fortran) = [srcSpan fortran]

getFirstBlockSrc :: Block Anno -> [SrcSpan]
getFirstBlockSrc codeSeg = [srcSpan codeSeg]

produceCodeBlock :: KernelArgsIndexMap -> ArgumentTranslation -> (Program Anno, String) -> String -> [String] -> Maybe(Fortran Anno) -> Block Anno -> String
produceCodeBlock allKernelArgsMap argTranslation prog tabs originalLines maybePrefix (Block anno useBlock imp src decl fort) 	
																		|	nonGeneratedHeader_ls < 1 = error "produceCodeBlock: nonGeneratedHeader_ls < 1"
																		|	nonGeneratedFooter_ls < 1 = error "produceCodeBlock: nonGeneratedFooter_ls < 1"
																		|	otherwise =	nonGeneratedHeaderCode
																			++	bufferDeclarationStatements ++ "\n"
																			++	statePtrDeclStr ++ "\n"
																			++	sizeDeclarations ++ "\n"
																			++	(if openCLReducePresent then nonGeneratedBlockCode_indent ++ reductionIteratorDecl ++ "\n" else "")
																			++	prefixString
																			++	shapeStatements ++ "\n"
																			++	loadBufferStatements ++ "\n"
																			++ 	produceCode_fortran prog tabs originalLines fort
																			-- ++	"! Start of footer\n"
																			++	nonGeneratedFooterCode
																			-- ++	"! End of block\n"
		where
			block = Block anno useBlock imp src decl fort
			fortranSrc = srcSpan fort
			((SrcLoc _ fortran_ls _), _) = fortranSrc
			(nonGeneratedHeaderSrc, nonGeneratedFooterSrc) = getSrcSpanNonIntersection src fortranSrc

			((SrcLoc _ nonGeneratedHeader_ls _), (SrcLoc _ nonGeneratedHeader_le _)) = nonGeneratedHeaderSrc
			nonGeneratedHeaderCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedHeader_ls..nonGeneratedHeader_le-1]
			
			((SrcLoc _ nonGeneratedFooter_ls _), (SrcLoc _ nonGeneratedFooter_le _)) = nonGeneratedFooterSrc
			nonGeneratedFooterCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedFooter_ls+1..nonGeneratedFooter_le-1]

			nonGeneratedBlockCode_indent = extractIndent (originalLines!!(fortran_ls-1))

			(sizeDeclarations, shapeStatements) = synthesiseSizeStatements_kernel nonGeneratedBlockCode_indent (fst prog)
			(bufferDeclarationStatements, loadBufferStatements) = synthesiseBufferLoads_kernel nonGeneratedBlockCode_indent allKernelArgsMap argTranslation block
			statePtrDeclStr = synthesiseDecl tabs statePtrDecl

			openCLReducePresent = (extractOpenCLReduces block) /= []

			prefixString = case maybePrefix of
								Just fortPrefix -> produceCode_fortran prog tabs originalLines fortPrefix
								Nothing -> ""

--	This function is called very often. It is the default when producing the body of each of the kernels and calls other functions
--	based on the node in the AST that it is called against. Each of the 'synthesise...' functions check whether the node in question
--	is a 'generated' node to determine whether or not code from the original file can be used.
produceCode_fortran :: (Program Anno, String) -> String -> [String] -> Fortran Anno -> String
produceCode_fortran prog tabs originalLines codeSeg = case codeSeg of
						If _ _ _ _ _ _ -> synthesiseIf prog tabs originalLines codeSeg
						Assg _ _ _ _ -> synthesiseAssg prog tabs originalLines codeSeg
						For _ _ _ _ _ _ _ -> synthesiseFor prog tabs originalLines codeSeg
						NullStmt _ _ -> ""
						OpenCLBufferRead _ _ _ -> synthesiseOpenCLBufferRead prog tabs originalLines codeSeg
						OpenCLBufferWrite _ _ _ -> synthesiseOpenCLBufferWrite prog tabs originalLines codeSeg
						OpenCLMap _ _ _ _ _ _ -> (synthesiseKernelCall prog tabs codeSeg) ++ (commentSeparator "END")
						OpenCLReduce _ _ _ _ _ rv f -> (synthesiseKernelCall prog tabs codeSeg)
																++ (mkQ "" (produceCode_fortran prog tabs originalLines) hostReductionLoop) ++ "\n" ++ (commentSeparator "END")
								where 
									reductionVarNames = map (\(varname, expr) -> varname) rv
									r_iter = generateReductionIterator
									-- r_iter = generateReductionIterator reductionVarNames
									hostReduction = generateFinalHostReduction reductionVarNames r_iter f
									hostReductionLoop = generateLoop r_iter (generateIntConstant 1) nunitsVar hostReduction
						Call _ _ _ _ -> synthesiseCall prog tabs originalLines codeSeg
						FSeq _ _ fortran1 fortran2 -> (mkQ "" (produceCode_fortran prog tabs originalLines) fortran1) ++ (mkQ "" (produceCode_fortran prog tabs originalLines) fortran2)
						_ -> 	case anyChildGenerated codeSeg || isGenerated codeSeg of
									True -> foldl (++) tabs (gmapQ (mkQ "" (produceCode_fortran prog "" originalLines)) codeSeg)
									False -> extractOriginalCode tabs originalLines (srcSpan codeSeg)

synthesiseCall :: (Program Anno, String) -> String -> [String] -> Fortran Anno -> String
synthesiseCall prog tabs originalLines (Call anno src expr args)	|	partialGenerated = prefix ++ tabs ++ "call " ++ (outputExprFormatting expr) ++ (synthesiseArgList args) ++ suffix ++ "\n"
																	|	otherwise = prefix ++ (extractOriginalCode tabs originalLines src) ++ suffix
		where
			partialGenerated = anyChildGenerated codeSeg || isGenerated codeSeg
			codeSeg = (Call anno src expr args)
			subroutineName = (outputExprFormatting expr)
			prefix = "" -- if subroutineName == initSubroutineName then "\n" ++ (commentSeparator "BEGIN OPENCL") else ""
			suffix = "" -- if subroutineName == tearDownSubroutineName then "\n" ++ (commentSeparator "END OPENCL") else ""

synthesiseArgList :: ArgList Anno -> String
synthesiseArgList (ArgList _ expr) = "(" ++ (synthesiseESeq expr) ++ ")"

synthesiseESeq :: Expr Anno -> String
synthesiseESeq (ESeq _ _ expr1 expr2) = (synthesiseESeq expr1) ++ "," ++ (synthesiseESeq expr2)
synthesiseESeq expr = outputExprFormatting expr

synthesisUses :: String -> Uses Anno -> String
synthesisUses tabs (Use _ (str, rename) _ _) = tabs ++ "use " ++ str ++ "\n"
synthesisUses tabs _ = ""

synthesiseOpenCLBufferWrite :: (Program Anno, String) -> String -> [String] -> Fortran Anno -> String
synthesiseOpenCLBufferWrite (progAst, filename) tabs originalLines (OpenCLBufferWrite anno src varName) = bufferWrite ++ "\n"
		where
			bufferWrite = synthesiseBufferAccess tabs "Write" writeDecl
			(_, writeDecl:_, _) = generateKernelDeclarations progAst (OpenCLBufferWrite anno src varName)

synthesiseOpenCLBufferRead :: (Program Anno, String) -> String -> [String] -> Fortran Anno -> String
synthesiseOpenCLBufferRead (progAst, filename) tabs originalLines (OpenCLBufferRead anno src varName) = bufferRead ++ "\n"
		where
			bufferRead = synthesiseBufferAccess tabs "Read" readDecl
			(readDecl:_, _, _) = generateKernelDeclarations progAst (OpenCLBufferRead anno src varName)

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

getDimensionExprs :: Attr Anno -> [(Expr Anno, Expr Anno)]
getDimensionExprs (Dimension _ exprs) = exprs
getDimensionExprs _ = []

insertDecls :: [Decl Anno] -> Decl Anno -> Decl Anno
insertDecls newDecls declTree = insertDecl newDeclsTree declTree
		where
			newDeclsTree = constructDeclTree newDecls

constructDeclTree :: [Decl Anno] -> Decl Anno
constructDeclTree [] = NullDecl nullAnno nullSrcSpan
constructDeclTree (decl:[]) = decl
constructDeclTree (decl:decls) = DSeq nullAnno decl (constructDeclTree decls)

--	Given a decl to insert and the start of the decl tree, insert the new decl and return the new decl tree
insertDecl :: Decl Anno -> Decl Anno -> Decl Anno 
insertDecl newDecl (DSeq anno decl1 decl2) = DSeq anno decl1 (insertDecl newDecl decl2)
insertDecl newDecl declLeaf = DSeq nullAnno declLeaf newDecl

synthesiseDecl :: String -> Decl Anno -> String
synthesiseDecl tabs (Decl anno src lst typ) = tabs ++ (synthesiseType typ) ++ " :: " ++ (synthesiseDeclList lst) ++ "\n"
synthesiseDecl tabs (DSeq _ decl1 decl2) = (synthesiseDecl tabs decl1) ++ (synthesiseDecl tabs decl2)
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
synthesiseAttrList attrList = if attrStrList == [] then error "synthesiseAttrList" else attrStrs
				where 
					attrStrList = map (synthesiseAttr) (filter (\x -> not (paramCheck_attr x)) attrList)
					attrStrs = foldl (\accum item -> accum ++ ", " ++ item) (head attrStrList) (tail attrStrList)

synthesiseAttr :: Attr Anno -> String
synthesiseAttr (Dimension _ exprList) = "dimension(" ++ synthesiseRangeExpr exprList ++ ")"
synthesiseAttr (Intent _ intentAttr) = "intent(" ++ intentStr ++ ")"
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
												
												(readDecls, writtenDecls, generalDecls) = adaptForScalarArgs (generateKernelDeclarations prog (OpenCLMap anno src r w l fortran))
												-- (readDecls, writtenDecls, generalDecls) = generateKernelDeclarations prog (OpenCLMap anno src r w l fortran)

												readDeclStr = foldl (\accum item -> accum ++ synthesiseDecl (tabs) item) "" (readDecls)
												writtenDeclStr = foldl (\accum item -> accum ++ synthesiseDecl (tabs) item) "" (writtenDecls)
												generalDeclStr = foldl (\accum item -> accum ++ synthesiseDecl (tabs) item) "" (generalDecls)

												globalIdDeclaration = "integer :: " ++ outputExprFormatting globalIdVar ++ "\n"
												globalIdInitialisation = "call " ++ outputExprFormatting (getGlobalID globalIdVar) ++ "\n"

												loopInitialisers = generateLoopInitialisers l globalIdVar Nothing
												loopInitialiserCode = case loopInitialisers of
																		[] -> error "synthesiseOpenCLMap: loopInitialiserCode - empty list"
																		_ -> foldl1 (\accum item -> appendFortran_recursive item accum) loopInitialisers

adaptForScalarArgs :: ([Decl Anno], [Decl Anno], [Decl Anno]) -> ([Decl Anno], [Decl Anno], [Decl Anno])
adaptForScalarArgs (readDecls, writtenDecls, generalDecls) = (readDecls, writtenDecls, generalDecls)

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
																			-- ++ global_reductionArraysDeclStr
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
												
												(readDecls, writtenDecls, generalDecls) = generateKernelDeclarations prog (OpenCLReduce anno src r w l rv fortran)
												
												readDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" (readDecls)
												writtenDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" (writtenDecls)
												generalDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" (generalDecls)
												
												local_reductionVars = map (generateLocalReductionVar) reductionVarNames
												local_reductionVarsInitStr = foldl (\accum (var, expr) -> accum ++ tabs ++ "local_" ++ varNameStr var ++ " = " ++ outputExprFormatting expr ++ "\n") "" rv
												local_reductionVarsDeclatation = map (\(red, local) -> stripDeclAttrs $ adaptOriginalDeclaration_varname red local prog) (zip reductionVarNames local_reductionVars)
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

												reductionIterator = generateReductionIterator
												-- reductionIterator = generateReductionIterator (r ++ w ++ (map (\(x,_,_,_) -> x) l) ++ reductionVarNames)
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
												workGroup_reductionArraysDeclStr = synthesiseDecls tabs workGroup_reductionArraysDecl
												workGroup_reductionArraysInitStr = foldl (generateReductionArrayAssignment tabs localIdFortranVar) "" (zip workGroup_reductionArrays local_reductionVars)
												workGroup_reductionCode = generateWorkGroupReduction reductionVarNames reductionIterator fortran
												workGroup_loop = generateLoop reductionIterator (generateIntConstant 1) nthVar workGroup_reductionCode

												global_reductionArrays = map (generateGlobalReductionArray) reductionVarNames
												-- global_reductionArraysDecl = map (\x -> declareGlobalReductionArray x (nunitsVar) prog) reductionVarNames
												-- global_reductionArraysDeclStr = synthesiseDecls tabs global_reductionArraysDecl
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
statePtrVarName = VarName nullAnno "state_ptr"
statePtrDecl = Decl nullAnno nullSrcSpan [(statePtrVar, NullExpr nullAnno nullSrcSpan, Nothing)] 
									(BaseType nullAnno (Integer nullAnno) [Dimension nullAnno [(NullExpr nullAnno nullSrcSpan, generateIntConstant 1)]] (NullExpr nullAnno nullSrcSpan) (NullExpr nullAnno nullSrcSpan))
stateVar = generateVar stateVarName
stateVarDecl = Decl nullAnno nullSrcSpan [(stateVar, NullExpr nullAnno nullSrcSpan, Nothing)] 
									(BaseType nullAnno (Integer nullAnno) [] (NullExpr nullAnno nullSrcSpan) (NullExpr nullAnno nullSrcSpan))
statePtrVar = generateVar statePtrVarName
-- statePtrDecl = Decl nullAnno nullSrcSpan 
-- 					[(statePtrVar, NullExpr nullAnno nullSrcSpan, Nothing)] 
-- 					(BaseType nullAnno 
-- 						(Integer nullAnno) 
-- 						[(Dimension 
-- 							nullAnno
-- 							[(generateIntConstant 1, generateIntConstant 256)])]
-- 						(NullExpr nullAnno nullSrcSpan) 
-- 						(NullExpr nullAnno nullSrcSpan))
initModuleName moduleName = moduleName ++ "_init"
hostModuleName moduleName = moduleName ++ "_host"

scalarPointerVar :: VarName Anno -> Expr Anno
scalarPointerVar varname = generateVar (scalarPointerVarName varname)

scalarPointerVarName :: VarName Anno -> VarName Anno
scalarPointerVarName (VarName _ str) = VarName nullAnno (str ++ "_ptr")

varSizeVar :: VarName Anno -> Expr Anno
varSizeVar varName = generateVar (varSizeVarName varName)

varSizeVarName :: VarName Anno -> VarName Anno
varSizeVarName (VarName _ str) = VarName nullAnno (str ++ "_sz")

varBufVar :: VarName Anno -> Expr Anno
varBufVar varName = generateVar (varSizeVarName varName)

varBufVarName :: VarName Anno -> VarName Anno
varBufVarName (VarName _ str) = VarName nullAnno (str ++ "_buf")


generateLocalReductionArray (VarName anno str) = VarName anno ("local_" ++ str ++ "_array")
generateGlobalReductionArray (VarName anno str) = VarName anno ("global_" ++ str ++ "_array")
generateLocalReductionArrayArgStr accum item = accum ++ "\n" ++ tabInc ++ "__local " ++ varNameStr item
generateGloablReductionArrayArgStr accum item = accum ++ "\n" ++ tabInc ++ "__global " ++ varNameStr item
generateLocalReductionVar (VarName anno str) = VarName anno ("local_" ++ str)

generateReductionArrayAssignment tabs accessor accum ((VarName _ s1),(VarName _ s2)) = accum++tabs++s1++"("++(outputExprFormatting accessor)++") = "++s2++"\n"

generateImplicitDecl :: VarName Anno -> Decl Anno
generateImplicitDecl var = Decl nullAnno nullSrcSpan [(generateVar var, (NullExpr nullAnno nullSrcSpan), Nothing)] (BaseType nullAnno (Real nullAnno) [] (NullExpr nullAnno nullSrcSpan) (NullExpr nullAnno nullSrcSpan))

--	The following functions are used to define names for output files from the input files' names.
getModuleName :: String -> String
getModuleName filename = head (splitOnChar '.' (last (splitOnChar '/' filename)))

splitOnChar :: Char -> String -> [String]
splitOnChar char str = splitOnChar' char "" str

splitOnChar' :: Char -> String -> String -> [String]
splitOnChar' char current (x:xs) 	|	char == x = current:(splitOnChar' char "" xs)
									|	otherwise = splitOnChar' char (current ++ [x]) xs
splitOnChar' _ current []			=	[current]

generateKernelName :: String -> SrcSpan -> [VarName Anno] -> String
generateKernelName identifier src varnames = (getModuleName filename) ++ "_" ++ identifier
											++ "_" ++ show line
			where
				((SrcLoc filename line _), _) = src

determineSubroutineName :: Program Anno -> SrcSpan -> String
determineSubroutineName ast src 	|	length matchingSubroutineNamesSrcs > 1 = error "determineSubroutineName: length matchingSubroutineNamesSrcs > 1"
									|	matchingSubroutineNamesSrcs == [] = error ("determineSubroutineName: matchingSubroutineNamesSrcs == []\n\nAST:\n\n" ++ (show ast) ++ "\n\nsrc:\n\n" ++ (show src))
									|	otherwise = fst (head matchingSubroutineNamesSrcs)
		where
			subroutineHeaders = extractSubroutines ast
			subroutineNamesSrcs = map (\x -> (extractProgUnitName x, srcSpan x)) subroutineHeaders
			matchingSubroutineNamesSrcs = filter (\(_, s) -> checkSrcSpanContainsSrcSpan s src) subroutineNamesSrcs

--	Function used during host code generation to produce call to OpenCL kernel.
synthesiseKernelCall :: (Program Anno, String) -> String -> Fortran Anno -> String
synthesiseKernelCall (progAst, filename) tabs (OpenCLMap anno src r w l fortran) = (commentSeparator ("BEGIN " ++ kernelName))
														++ tabs ++ "oclGlobalRange = " ++ outputExprFormatting globalWorkItems ++ "\n"
														++ tabs ++ (varNameStr statePtrVarName) ++ "(1) = " ++ stateName ++ "\n"
														++ tabs ++ bufferWrites ++ "\n"
														++ tabs ++ "call runOcl(oclGlobalRange,oclLocalRange,exectime)\n"
														++ tabs ++ "! call " ++ kernelName
														++ bufferReads ++ "\n"
			where
				readArgs = map (varNameStr) (listSubtract r w)
				writtenArgs = map (varNameStr) (listSubtract w r)
				generalArgs = map (varNameStr) (listIntersection w r)

				allArguments = readArgs ++ writtenArgs ++ generalArgs
				allArgumentsStr = case allArguments of
									[] -> "NO ARGS"
									_ -> (head allArguments) ++ foldl (\accum item -> accum ++ "," ++ item) "" (tail allArguments)

				globalWorkItems = generateGlobalWorkItemsExpr l

				kernelName = (generateKernelName "map" src w)
				stateName = generateStateName kernelName

				bufferWrites = foldl (\accum item -> accum ++ "\n" ++ item) "" (map (synthesiseBufferAccess tabs "Write") (readDecls ++ generalDecls ++ [statePtrDecl]))
				bufferReads = foldl (\accum item -> accum ++ "\n" ++ item) "" (map (synthesiseBufferAccess tabs "Read") (writtenDecls ++ generalDecls))

				(readDecls, writtenDecls, generalDecls) = generateKernelDeclarations progAst (OpenCLMap anno src r w l fortran)
synthesiseKernelCall (progAst, filename) tabs (OpenCLReduce anno src r w l rv fortran) = (commentSeparator ("BEGIN " ++ kernelName))
															++ tabs ++ "oclGlobalRange = " ++ outputExprFormatting reductionWorkItemsExpr ++ "\n"
															++ tabs ++ "oclLocalRange = " ++ outputExprFormatting nthVar ++ "\n"
															++ tabs ++ "ngroups = " ++ outputExprFormatting nunitsVar ++ "\n"
															++ tabs ++ (varNameStr statePtrVarName) ++ "(1) = " ++ stateName ++ "\n"
															++ tabs ++ bufferWrites ++ "\n\n"
															++ tabs ++ "call runOcl(oclGlobalRange,oclLocalRange,exectime)\n"
															++ tabs ++ "! call " ++ kernelName
														 	++ bufferReads
															++ bufferReads_rv 
															++ "\n"
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
				allArgumentsStr = case allArguments of
									[] -> "NO ARGS"
									_ -> (head allArguments) ++ foldl (\accum item -> accum ++ "," ++ item) "" (tail allArguments)

				reductionWorkItemsExpr = generateProductExpr nthVar nunitsVar

				kernelName = (generateKernelName "reduce" src (map (\(v, e) -> v) rv)) 
				stateName = generateStateName kernelName

				bufferWrites = foldl (\accum item -> accum ++ "\n" ++ item) "" (map (synthesiseBufferAccess tabs "Write") (readDecls ++ generalDecls ++ [statePtrDecl]))
				bufferReads = foldl (\accum item -> accum ++ "\n" ++ item) "" (map (synthesiseBufferAccess tabs "Read") (writtenDecls ++ generalDecls))
				bufferReads_rv = foldl (\accum item -> accum ++ "\n" ++ item) "" (map (synthesiseBufferAccess tabs "Read") (global_reductionArraysDecls))

				(readDecls, writtenDecls, generalDecls) = generateKernelDeclarations progAst (OpenCLReduce anno src r w l rv fortran)

				global_reductionArraysDecls = map (\x -> declareGlobalReductionArray x (nunitsVar) progAst) reductionVarNames

--	Produce code for a buffer read or write. If a scalar variable declaration is supplied, it is automatically converted to an array to be
--	used in the buffer access.
synthesiseBufferAccess :: String -> String -> Decl Anno -> String
synthesiseBufferAccess tabs method (Decl anno src lst typ) = case baseType of
														Integer _ -> prefix ++ "Int" ++ suffix
														Real _ -> prefix ++ "Float" ++ suffix
														_ -> ""
			where
				assignee = extractAssigneeFromDecl (Decl anno src lst typ)
				varStr = varNameStr assignee
				varBufStr = varNameStr (varBufVarName assignee)

				baseType = extractBaseType typ
				declRank = getDeclRank (Decl anno src lst typ)

				isScalar = declRank == 0
				dimensions = if isScalar then 1 else declRank
				scalarPointerConversion = if isScalar then tabs ++ (varNameStr (scalarPointerVarName assignee)) ++ "(1) = " ++  varStr ++ "\n" else ""
				varSzStr = if isScalar then (varNameStr (varSizeVarName (scalarPointerVarName assignee))) else varNameStr (varSizeVarName assignee)
				bufferAccessSubjectVarStr = if isScalar then (varNameStr (scalarPointerVarName assignee)) else varStr
				

				prefix = scalarPointerConversion ++ tabs ++ "call ocl" ++ method ++ (show dimensions) ++ "D"
				suffix = "ArrayBuffer(" ++ varBufStr ++ "," ++ varSzStr ++ "," ++ bufferAccessSubjectVarStr ++ ")" ++ (if isScalar then "! Automatic conversion to array" else "")
synthesiseBufferAccess _ _ _ = error "synthesiseBufferAccess"

synthesiseBufferMake :: String -> Decl Anno -> String
synthesiseBufferMake tabs (Decl anno src lst typ) = case baseType of
														Integer _ -> prefix ++ "Int" ++ suffix
														Real _ -> prefix ++ "Float" ++ suffix
														_ -> ""
			where
				assignee = extractAssigneeFromDecl (Decl anno src lst typ)
				varStr = varNameStr assignee
				varBufStr = varNameStr (varBufVarName assignee)

				baseType = extractBaseType typ
				declRank = getDeclRank (Decl anno src lst typ)

				isScalar = declRank == 0
				dimensions = if isScalar then 1 else declRank
				scalarPointerConversion = -- if isScalar then tabs ++ (varNameStr (scalarPointerVarName assignee)) ++ "(1) = " ++  varStr ++ "\n" else 
											""
				varSzStr = if isScalar then (varNameStr (varSizeVarName (scalarPointerVarName assignee))) else varNameStr (varSizeVarName assignee)
				bufferAccessSubjectVarStr = if isScalar then (varNameStr (scalarPointerVarName assignee)) else varStr
				

				prefix = scalarPointerConversion ++ tabs ++ "call oclMake" ++ (show dimensions) ++ "D"
				suffix = "ArrayReadWriteBuffer(" ++ varBufStr ++ "," ++ varSzStr ++ "," ++ bufferAccessSubjectVarStr ++ ")" ++ (if isScalar then "! Automatic conversion to array" else "")
synthesiseBufferMake _ _ = error "synthesiseBufferMake"

getDeclRank :: Decl Anno -> Int
getDeclRank decl 	|	extractedDimensions == [] = 0
					|	otherwise = dimensionRank
		where
			extractedDimensions = everything (++) (mkQ [] extractDimensionAttr) decl
			dimensionRank = length (getDimensionExprs (head extractedDimensions))

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

declareGlobalReductionArray :: VarName Anno -> Expr Anno -> Program Anno -> Decl Anno
declareGlobalReductionArray varname arraySize program = decl			
			where
				decl_list = extractDeclaration_varname varname program
				foundDecl = case decl_list of
								[] -> generateImplicitDecl varname
								_ -> head decl_list
				one = generateIntConstant 1
				newVarName = generateGlobalReductionArray varname
				decl = applyIntent (Out nullAnno) (replaceAllOccurences_varname (addDimension foundDecl one arraySize) varname newVarName)

declareScalarPointer :: VarName Anno -> Program Anno -> Decl Anno
declareScalarPointer varname program = decl
			where
				decl_list = extractDeclaration_varname varname program
				foundDecl = case decl_list of
								[] -> generateImplicitDecl varname
								_ -> head decl_list
				one = generateIntConstant 1
				nullExpr = NullExpr nullAnno nullSrcSpan
				newVarName = scalarPointerVarName varname
				decl = replaceAllOccurences_varname (addDimension foundDecl nullExpr one) varname newVarName

declareScalarPointer_decl :: Decl Anno -> Decl Anno
declareScalarPointer_decl decl = resultDecl
			where
				one = generateIntConstant 1
				zero = generateIntConstant 0
				varname = extractAssigneeFromDecl decl
				newVarName = scalarPointerVarName varname
				resultDecl = replaceAllOccurences_varname (addDimension decl zero one) varname newVarName

declareLocalReductionArray :: VarName Anno -> Expr Anno -> Program Anno -> Decl Anno
declareLocalReductionArray varname arraySize program = decl
			where
				decl_list = extractDeclaration_varname varname program
				foundDecl = case decl_list of
								[] -> generateImplicitDecl varname
								_ -> head decl_list
				one = generateIntConstant 1
				newVarName = generateLocalReductionArray varname
				decl = addDimension (replaceAllOccurences_varname foundDecl varname newVarName) one arraySize

adaptOriginalDeclaration_varname :: VarName Anno -> VarName Anno -> Program Anno -> Decl Anno
adaptOriginalDeclaration_varname varname newVarname program = decl
			where 
				decl_list = extractDeclaration_varname varname program
				foundDecl = case decl_list of
								[] -> generateImplicitDecl varname
								_ -> head decl_list
				decl = applyGeneratedSrcSpans (replaceAllOccurences_varname foundDecl varname newVarname)



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
				decl_list = extractDeclaration_varname varname program
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

removeIntentFromDecl :: Decl Anno -> Decl Anno
removeIntentFromDecl decl = gmapT (mkT removeIntentFromType) decl

removeIntentFromType :: Type Anno -> Type Anno
removeIntentFromType (BaseType anno btype attrList expr1 expr2) = (BaseType anno btype newAttrList expr1 expr2)
		where
			newAttrList = filter (\x -> not (isIntent x)) attrList
removeIntentFromType (ArrayT  anno exprList btype attrList expr1 expr2) = (ArrayT  anno exprList btype newAttrList expr1 expr2)
		where
			newAttrList = filter (\x -> not (isIntent x)) attrList

isIntent :: Attr Anno -> Bool
isIntent (Intent _ _) = True
isIntent _ = False

-- data Type     p = BaseType p                    (BaseType p) [Attr p] (Expr p) (Expr p)
--                 | ArrayT   p [(Expr p, Expr p)] (BaseType p) [Attr p] (Expr p) (Expr p)

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

extractDeclaration_varname :: VarName Anno -> Program Anno -> [Decl Anno]
extractDeclaration_varname varname program = everything (++) (mkQ [] (extractDeclaration_varname' varname)) program

extractDeclaration_varname' :: VarName Anno -> Decl Anno -> [Decl Anno]
extractDeclaration_varname' varname  (Decl anno src lst typ)  	| firstHasVar || secondHasVar = [Decl anno src lst typ]
														| otherwise = []
			where
				firstExprs = map (\(expr, _, _) -> expr) lst
				secondExprs = map (\(_, expr, _) -> expr) lst

				firstHasVar = foldl (\accum item -> accum || usesVarName_list [varname] item) False firstExprs
				secondHasVar = foldl (\accum item -> accum || usesVarName_list [varname] item) False secondExprs
extractDeclaration_varname' varname decl = []

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
generateWorkGroupReduction reductionVars redIter codeSeg  = if assignments == [] then error ("generateWorkGroupReduction: assignments == []\nrv: " ++ (show reductionVars) ++ "\ncodeseg: " ++ (show codeSeg)) else resultantCode
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

-- Formally, this function produced a varname that had not been used in a supplied list of varnames, to ensure that the iterator
-- variable didn't clash. This feature has been scrapped for the time being as it causes complications. 
generateReductionIterator :: VarName Anno
generateReductionIterator = VarName nullAnno "r_iter"

reductionIteratorDecl :: String
reductionIteratorDecl = "Integer :: " ++ (varNameStr generateReductionIterator)

-- generateReductionIterator :: [VarName Anno] -> VarName Anno
-- generateReductionIterator usedNames = VarName nullAnno choice
			-- where
			-- 	possibles = ["reduction_iterator", "reduction_iter", "r_iter"]
			-- 	choice = case possibles of
			-- 			[] -> error "generateReductionIterator: choice - empty list"
			-- 			_ -> foldl1 (\accum item -> if not (elem (VarName nullAnno item) usedNames) then item else accum) possibles

tabInc :: String
tabInc = "    "