module FortranSynthesiser 

where

-- The intention of this module is to house all of the functions that take some AST node and produce a final String
-- representation that will be used as correct Fortran 95 code. The module also includes functions to generate names
-- for modules and suproutines. Most functions are fairly straightforward but some look rather packed. In the cases 
-- of larger functions, I've tried to make it very clear which variables are used (and in which order) for the final
-- output from the function. 

-- Most, if not all, of the functions have a 'tabs' argument. This is purely for presentation as it allows the current
-- indentation in the output source to be tracked. 

-- In some cases, functions for very simple AST nodes, or AST nodes that are unlikely to differ very much simple output 
-- a mostly hardcoded string. In more complicated cases, the whole string representation of a node is formed through 
-- recursive calls to more modular functions.

-- The most interesting function is 'synthesiseOpenCLReduce' as it requires lots of transformation of original code, including
-- the insertion of partial reductions for the sake of effective GPU use. The function is explained in more detail in a header
-- comment. The 'synthesiseOpenCLReduce' function is one of many that produce code from AST nodes that would not exist in 
-- normal Fortran. Another example is 'synthesiseKernelCall'

import Data.Generics 					(Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran
import Data.Char
import Data.List
import qualified Data.Map as DMap 

import FortranGenerator
import CodeEmitterUtils
import LanguageFortranTools
import SubroutineTable 					(ArgumentTranslation, ArgumentTranslationSubroutines, emptyArgumentTranslation, getSubroutineArgumentTranslation, translateArguments,
										extractSubroutines, extractProgUnitName)


--	This function produces a list of strings where each element is a line of the original source. This
--	list is used heavily in this module.
readOriginalFileLines :: [String] -> Bool -> String -> IO ([String])
readOriginalFileLines cppDFlags fixedForm filename = do
				content <- cpp cppDFlags fixedForm filename
				let contentLines = lines content
				return contentLines

defaultFilename :: [String] -> String
defaultFilename (x:[]) = "par_" ++ x
defaultFilename (x:xs) = x ++ "/" ++ defaultFilename xs

generateStateName :: String -> String
generateStateName kernelName = "ST_" ++ (map (toUpper) kernelName)

generateKernelName :: String -> SrcSpan -> [VarName Anno] -> String
generateKernelName identifier src varnames = (getModuleName filename) ++ "_" ++ identifier
											++ "_" ++ show line
			where
				((SrcLoc filename line _), _) = src

generateOriginalFileName :: [String] -> String
generateOriginalFileName (x:[]) = "original_" ++ x
generateOriginalFileName (x:xs) = x ++ "/" ++ generateOriginalFileName xs

synthesiseSuperKernelName :: [String] -> String
synthesiseSuperKernelName originalFilenames = base ++ suffix
		where
			maxLen = 30
			base = take maxLen (map (toLower) ((foldl1 (\accum item -> accum ++ "_" ++ item) originalFilenames)))
			suffix = if (length base == maxLen) 
				then 
					(case (last base) of
									'_' -> "etc_superkernel"
									_ -> "_etc_superkernel")
				else 
					(case (last base) of
									'_' -> "superkernel"
									_ -> "_superkernel")

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
												++ 	everything (++) (mkQ "" (produceCodeBlock allKernelArgsMap argTranslation progWithFilename nonGeneratedBlockCode_indent originalLines Nothing)) block
												++ 	nonGeneratedFooterCode	
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

produceCode_progUnit allKernelArgsMap argTranslationSubroutines progWithFilename kernelModuleName superKernelName originalLines progunit = foldl (++) "" (gmapQ (mkQ "" (produceCode_progUnit allKernelArgsMap argTranslationSubroutines progWithFilename kernelModuleName superKernelName originalLines)) progunit)


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

									hostReduction = generateFinalHostReduction reductionVarNames r_iter f
									hostReductionLoop = generateLoop r_iter (generateIntConstant 1) nunitsVar hostReduction
						Call _ _ _ _ -> synthesiseCall prog tabs originalLines codeSeg
						FSeq _ _ fortran1 fortran2 -> (mkQ "" (produceCode_fortran prog tabs originalLines) fortran1) ++ (mkQ "" (produceCode_fortran prog tabs originalLines) fortran2)
						_ -> 	case anyChildGenerated codeSeg || isGenerated codeSeg of
									True -> foldl (++) tabs (gmapQ (mkQ "" (produceCode_fortran prog "" originalLines)) codeSeg)
									False -> extractOriginalCode tabs originalLines (srcSpan codeSeg)

synthesiseInitModule :: String -> String ->  [(Program Anno, String)] -> KernelArgsIndexMap -> [(String, String)] -> String
synthesiseInitModule moduleName superKernelName programs allKernelArgsMap kernels = 	initModuleHeader 
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
																			++ contains
																			++ "\n"
																			++ (foldl (++) "" kernelCode) 
																			++ superKernelCode 
																			++ kernelModuleFooter,
															allKernelArgsMap)
				where
					kernelCode = map (fst) kernels

					kernelModuleHeader = "module " ++ moduleName ++ "\n\n"

					contains = "\n" ++ tabInc ++ "contains\n\n"
					kernelModuleFooter = "end module " ++ moduleName

					(superKernelCode, allKernelArgsMap) = synthesiseSuperKernel moduleName outputTab superKernelName programs kernels

synthesiseStateDefinitions :: [(String, String)] -> Int -> String
synthesiseStateDefinitions [] currentVal =  ""
synthesiseStateDefinitions ((kernel, state):xs) currentVal =  "integer, parameter :: " ++ state ++ " = " ++ show currentVal ++ " !  " ++ kernel ++ "\n" ++ (synthesiseStateDefinitions xs (currentVal+1))

synthesiseSuperKernel :: String -> String -> String -> [(Program Anno, String)] -> [(String, String)] -> (String, KernelArgsIndexMap)
synthesiseSuperKernel moduleName tabs name programs [] = ("", DMap.empty)
synthesiseSuperKernel moduleName tabs name programs kernels = if allKernelArgs == [] then error "synthesiseSuperKernel" else (superKernel, allKernelArgsMap)
				where
					programAsts = map (fst) programs
					kernelAstLists = map (extractKernels) programAsts

					extractedUses = foldl (\accum prog -> listConcatUnique accum (everything (++) (mkQ [] getUses) prog)) [] programAsts
					useStatements = "use " ++ initModuleName moduleName ++ "\n" 
										++ (foldl (\accum item -> synthesiseUse ([], "") "" [] item) "" extractedUses)

					kernelNames = map (snd) kernels
					stateNames = map (generateStateName) kernelNames
					stateDefinitions = synthesiseStateDefinitions (zip kernelNames stateNames) 0

					kernelAsts = foldl (++) [] (map (\(k_asts, p_ast) -> map (\a -> (a, p_ast)) k_asts) (zip kernelAstLists programAsts))
					kernelArgs = (map (\(x, _) -> extractKernelArguments x) kernelAsts)
					allKernelArgs = (listRemoveDuplications (foldl (++) [] kernelArgs)) ++ [statePtrVarName]

					allKernelArgsMap = foldl (\dmap (arg, index) -> DMap.insert arg index dmap) DMap.empty (zip allKernelArgs ([1..(length allKernelArgs)]))

					kernelDeclarations = map (\(kernel_ast, prog_ast) -> generateKernelDeclarations prog_ast kernel_ast) kernelAsts
					(readDecls, writtenDecls, generalDecls) =  foldl (\(accum_r, accum_w, accum_g) (r, w, g) -> (accum_r ++ r, accum_w ++ w, accum_g ++ g)) ([],[],[]) kernelDeclarations

					declarations = map (convertScalarToOneDimArray) (foldl (collectDecls) [] (readDecls ++ writtenDecls ++ generalDecls))
					declarationsStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" declarations

					stateVarDeclStr = synthesiseDecl tabs stateVarDecl
					statePointerDeclStr = synthesiseDecl tabs statePtrDecl
					stateAssignment = tabs ++ (varNameStr stateVarName) ++ " = " ++ (varNameStr stateVarName) ++ "_ptr(1)\n"

					superKernel_header = "subroutine " ++ name ++ "(" ++ (foldl (\accum item -> accum ++ "," ++ (varNameStr item)) (varNameStr $ head allKernelArgs) (tail allKernelArgs)) ++ ")\n"
					superKernel_footer = "end subroutine " ++ name ++ "\n"
					superKernel_body = "! SUPERKERNEL BODY\n" ++ selectCase

					caseAlternatives = foldl (\accum (state, name, args) -> accum ++ synthesiseKernelCaseAlternative (tabs ++ outputTab) state name args) "" (zip3 stateNames kernelNames kernelArgs)
					selectCase = outputTab ++ "select case(" ++ (varNameStr stateVarName) ++ ")\n" ++ caseAlternatives ++ outputTab ++ "end select\n"

					superKernel = superKernel_header ++ useStatements ++ declarationsStr ++ "\n" ++ stateVarDeclStr ++ statePointerDeclStr ++ stateDefinitions ++ stateAssignment ++ superKernel_body ++ superKernel_footer

synthesiseKernelCaseAlternative :: String -> String -> String -> [VarName Anno] -> String
synthesiseKernelCaseAlternative tabs state kernelName [] = error "synthesiseKernelCaseAlternative"
synthesiseKernelCaseAlternative tabs state kernelName args =  tabs ++ "case (" ++ state ++ ")\n" ++ tabs ++ outputTab ++ "call " ++ kernelName ++ "(" ++ argsString ++ ")" ++ "\n" 
				where
					argsString = foldl (\accum item -> accum ++ "," ++ (varNameStr item)) (varNameStr $ head args) (tail args)

synthesiseKernels :: [String] -> (Program Anno, String) -> Fortran Anno -> [(String, String)]
synthesiseKernels originalLines prog codeSeg = case codeSeg of
				OpenCLMap _ src _ w _ _ -> [(synthesiseOpenCLMap "" originalLines prog codeSeg, generateKernelName "map" src w)]
				OpenCLReduce _ src _ _ _ rv _ ->  [(synthesiseOpenCLReduce "" originalLines prog codeSeg, generateKernelName "reduce" src (map (\(v, e) -> v) rv))]
				_ -> []

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

			varsWithRanks_arrays = map (\(var, rank) -> if rank == 0 then (scalarPointerVarName var, 1) else (var, rank)) varsWithRanks

			vars_onlyArrays = map (\(var, rank) -> if rank == 0 then scalarPointerVarName var else var) varsWithRanks
			vars_onlyScalars = listSubtract allVars vars_onlyArrays
			scalarPointerDeclarations = map (\x -> removeIntentFromDecl (declareScalarPointer x ast)) vars_onlyScalars

synthesiseSizeStatements_kernel :: String -> Program Anno -> (String, String)
synthesiseSizeStatements_kernel tabs ast = synthesiseSizeStatements tabs allBufferAccesses ast
		where
			kernels = extractKernels ast
			bufferReads = extractBufferReads ast
			bufferWrites = extractBufferWrites ast
			
			kernelArgs = listRemoveDuplications (foldl (\accum item -> accum ++ (extractKernelArguments item)) [] kernels)
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

synthesiseUse :: (Program Anno, String) -> String -> [String] -> Uses Anno -> String
synthesiseUse prog tabs originalLines (Use _ (moduleName, _) _ _) = tabs ++ "use " ++ moduleName ++ "\n"

synthesiseCall :: (Program Anno, String) -> String -> [String] -> Fortran Anno -> String
synthesiseCall prog tabs originalLines (Call anno src expr args)	|	partialGenerated = prefix ++ tabs ++ "call " ++ (outputExprFormatting expr) ++ (synthesiseArgList args) ++ suffix ++ "\n"
																	|	otherwise = prefix ++ (extractOriginalCode tabs originalLines src) ++ suffix
		where
			partialGenerated = anyChildGenerated codeSeg || isGenerated codeSeg
			codeSeg = (Call anno src expr args)
			subroutineName = (outputExprFormatting expr)
			prefix = ""
			suffix = ""

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

synthesiseDecl :: String -> Decl Anno -> String
synthesiseDecl tabs (Decl anno src lst typ) = tabs ++ (synthesiseType typ) ++ " :: " ++ (synthesiseDeclList lst) ++ "\n"
synthesiseDecl tabs (DSeq _ decl1 decl2) = (synthesiseDecl tabs decl1) ++ (synthesiseDecl tabs decl2)
synthesiseDecl tabs (NullDecl nullAnno nullSrcSpan) = tabs ++ "[Variable not declared in orignal source]\n"
synthesiseDecl tabs _ = tabs ++ "[Unimplemented declaration syntax] \n"

synthesiseDecls :: String -> [Decl Anno] -> String
synthesiseDecls tabs decls = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" decls

synthesiseDecl_Acc :: String -> Decl Anno -> String -> String
synthesiseDecl_Acc tabs (Decl anno src lst typ) acc = tabs ++ (synthesiseType typ) ++ " :: " ++ (synthesiseDeclList lst) ++ " " ++ acc ++ "\n"
synthesiseDecl_Acc tabs (DSeq _ decl1 decl2) acc = (synthesiseDecl_Acc tabs decl1 acc) ++ (synthesiseDecl_Acc tabs decl2 acc)
synthesiseDecl_Acc tabs decl _ = synthesiseDecl tabs decl

synthesiseDecls_Acc :: String -> [Decl Anno] -> String -> String
synthesiseDecls_Acc tabs decls acc = foldl (\accum item -> accum ++ synthesiseDecl_Acc tabs item acc) "" decls

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
																					++ allArgs_ptrAdaptionStr ++ ")\n"
																	++ usesString
																	++ "\n"
																	++ readDeclStr
																	++ writtenDeclStr
																	++ generalDeclStr
																	++ tabs ++ globalIdDeclaration
																	++ tabs ++ globalIdInitialisation
																 	++ (produceCode_fortran programInfo tabs originalLines ptrAssignments_fseq)
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

												allArgs = extractKernelArguments (OpenCLMap anno src r w l fortran)												
												(readDecls, writtenDecls, generalDecls, ptrAssignments_fseq, allArgs_ptrAdaption) = adaptForReadScalarDecls allArgs (generateKernelDeclarations prog (OpenCLMap anno src r w l fortran))
												allArgs_ptrAdaptionStr = case allArgs_ptrAdaption of
															[] -> ""
															args -> foldl (\accum item -> accum ++ "," ++ varNameStr item) (varNameStr (head args)) (tail args)

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
																			++ allArgs_ptrAdaptionStr
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

												allArgs = extractKernelArguments (OpenCLReduce anno src r w l rv fortran)
												
												(readDecls, writtenDecls, generalDecls, ptrAssignments, allArgs_ptrAdaption) = adaptForReadScalarDecls allArgs (generateKernelDeclarations prog (OpenCLReduce anno src r w l rv fortran))
												allArgs_ptrAdaptionStr = case allArgs_ptrAdaption of
															[] -> ""
															args -> foldl (\accum item -> accum ++ "," ++ varNameStr item) (varNameStr (head args)) (tail args)

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
												workGroup_reductionArraysDeclStr = synthesiseDecls_Acc tabs workGroup_reductionArraysDecl localMemSpaceAcc
												workGroup_reductionArraysInitStr = foldl (generateReductionArrayAssignment tabs localIdFortranVar) "" (zip workGroup_reductionArrays local_reductionVars)
												workGroup_reductionCode = generateWorkGroupReduction reductionVarNames reductionIterator fortran
												workGroup_loop = generateLoop reductionIterator (generateIntConstant 1) nthVar workGroup_reductionCode

												global_reductionArrays = map (generateGlobalReductionArray) reductionVarNames
												global_reductionArraysAssignmentStr = foldl (generateReductionArrayAssignment tabs groupIdFortranVar) "" (zip global_reductionArrays local_reductionVars)

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