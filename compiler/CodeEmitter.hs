module CodeEmitter 						(emit)

where

--	Code in this file handles the final emission of code. The function 'emit' is called against an AST that has been transformed and has had kernels fused.
--	Trys to make as much use as it can of code that is the same as it was in the original source file. Otherwise, it uses mostly simple functions to generate
--	code segments. 

--	Most of the heavily lifting is now performed by FortranSynthesiser.hs and FortranGenerator.hs, using functions found in CodeEmitterUtils.hs

import Control.Monad
import Data.Generics 					(Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List
import System.IO
import System.Process
import Data.Maybe
import qualified Data.Map as DMap 

import CodeEmitterUtils
import LanguageFortranTools
import SubroutineTable 					(ArgumentTranslation, ArgumentTranslationSubroutines, emptyArgumentTranslation, getSubroutineArgumentTranslation, translateArguments,
										extractSubroutines, extractProgUnitName)
import FortranSynthesiser

emit :: String -> [String] -> Bool -> [(Program Anno, String)] -> [(Program Anno, String)] -> ArgumentTranslationSubroutines -> (Program Anno, String) -> [VarName Anno] -> [VarName Anno] -> IO [()]
emit specified cppDFlags fixedForm programs_verboseArgs programs_optimisedBuffers argTranslations (mainAst, mainFilename) initWrites tearDownReads = do

				kernels_code <- mapM (emitKernels cppDFlags fixedForm) programs_verboseArgs
				let allKernels = foldl (++) [] kernels_code
				let kernelNames = map (snd) allKernels

				let originalFilenames = map (\x -> getModuleName (snd x)) programs_verboseArgs
				let superkernelName = synthesiseSuperKernelName originalFilenames
				let moduleName = "module_" ++ superkernelName
				let moduleFilename = specified ++ "/" ++ moduleName ++ ".f95"
				let newMainFilename = specified ++ "/" ++ (hostModuleName (getModuleName mainFilename)) ++ ".f95"
				let initModuleFilename = specified ++ "/" ++ (initModuleName moduleName) ++ ".f95"

				let (superKernel_module, allKernelArgsMap) = synthesiseSuperKernelModule moduleName superkernelName programs_verboseArgs allKernels
				let initModule = synthesiseInitModule moduleName superkernelName programs_verboseArgs allKernelArgsMap allKernels

				host_code <- mapM (produceCode_prog allKernelArgsMap argTranslations cppDFlags fixedForm moduleName superkernelName) programs_optimisedBuffers
				main_code <- produceCode_prog allKernelArgsMap argTranslations cppDFlags fixedForm moduleName superkernelName (mainAst, mainFilename)

				let host_programs = zip host_code (map (\x -> specified ++ "/" ++ x ++ "_host.f95") originalFilenames)

				writeFile moduleFilename (if fixedForm then fixedFormFormat superKernel_module else superKernel_module)
				writeFile newMainFilename (if fixedForm then fixedFormFormat main_code else main_code)
				writeFile initModuleFilename (if fixedForm then fixedFormFormat initModule else initModule)

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



emitKernels :: [String] -> Bool -> (Program Anno, String) -> IO [(String, String)]
emitKernels cppDFlags fixedForm (ast, filename) = do
				originalLines <- readOriginalFileLines cppDFlags fixedForm filename
				let originalListing = case originalLines of
										[]	-> ""
										_ -> foldl (\accum item -> accum ++ "\n" ++ item) (head originalLines) (tail originalLines)
				let kernels_code = everything (++) (mkQ [] (synthesiseKernels originalLines (ast, filename))) ast
				let kernels_renamed = map (\(code, kernelname) -> (code, kernelname)) kernels_code
				return kernels_renamed