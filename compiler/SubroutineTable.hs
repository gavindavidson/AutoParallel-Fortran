module SubroutineTable 				(SubroutineTable, ArgumentTranslationSubroutines, ArgumentTranslation, extractArgumentTranslationSubroutines, emptyArgumentTranslation, 
									generateArgumentTranslation, getSubroutineArgumentTranslation, constructSubroutineTable, replaceKernels_foldl, subroutineTable_ast, 
									extractCalls, extractAllCalls, translateArguments, extractSubroutines, extractProgUnitName)

where

import Data.Generics 				(Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import qualified Data.Map as DMap
import Language.Fortran

import LanguageFortranTools 


-- 	This datastructure holds subroutines, with the names of the subrouines as the key. The stored item is the AST of the subroutine
--	and the name of the file from which the subroutine was orignally defined.
type SubroutineTable = DMap.Map String (ProgUnit Anno, String)
subroutineTable_ast (a, _) =  a
subroutineTable_filename (_, b) =  b

--	These datastructures facilitate argument/variable translation. For each subroutine there is an 'ArgumentTranslation' structure
--	stored in the 'ArgumentTranslationSubroutines' structure. Each 'ArgumentTranslation' structure contains a set of VarName keys
--	and related VarName items. The VarName keys are the variables that appear in the current subroutine and the related items are
--	the names of the variables in terms of the main method (or whatever context the structure is constructed with). The structure
--	means that when a buffer is referenced in a subroutine in the final output code, first a check to this structure is made to be
--	sure that the variable in question doesn't exist under a different name/buffer number.
type ArgumentTranslationSubroutines = DMap.Map (String) ArgumentTranslation
type ArgumentTranslation = DMap.Map (VarName Anno) (VarName Anno)
emptyArgumentTranslation = DMap.empty

constructSubroutineTable :: [(Program Anno, String)] -> SubroutineTable
constructSubroutineTable programs = foldl (\accum (ast, filename) -> DMap.insert (extractProgUnitName ast) (ast, filename) accum) DMap.empty parsedSubroutines
		where
			parsedSubroutines = foldl (\accum (ast, filename) -> accum ++ (map (\x -> (x, filename)) (extractSubroutines ast))) [] programs

--	Function is called by main to compile the set of argument translation tables for all of the parsed subroutines.
--	STEPS:
--		-	Extract all calls to subroutines in the chosen AST (usually the main)
--		-	Call 'generateArgumentTranslationSubroutines' on each call:
--			+	Extract the appropriate subroutine from the subroutine table, if it exists
--			+	Extract args from the call and the subroutine definition
--			+	Match pairs of args from the call and definition according to the order in which they appear, adding
--				each pair to a Data.Map.
--			+	Return the Data.Map
--		-	Stored each Data.Map in another Data.Map, whose keys are subroutine names.
extractArgumentTranslationSubroutines subTable ast = argTransTable
		where
			subroutines = DMap.keys subTable
			calls = extractAllCalls ast
			argTransTable = foldl (generateArgumentTranslationSubroutines subTable) (DMap.empty) calls

translateArguments :: ArgumentTranslation -> [VarName Anno] -> [VarName Anno]
translateArguments argTranslations args = map (translateArgument argTranslations) args

translateArgument :: ArgumentTranslation -> VarName Anno -> VarName Anno
translateArgument argTranslations var = DMap.findWithDefault (var) var argTranslations

generateArgumentTranslationSubroutines :: SubroutineTable -> ArgumentTranslationSubroutines -> Fortran Anno -> ArgumentTranslationSubroutines
generateArgumentTranslationSubroutines subTable argTable (Call anno src callExpr arglist) = DMap.insert subroutineName varNameReplacements argTable
		where
			varNameReplacements = generateArgumentTranslation subTable (Call anno src callExpr arglist)
			subroutineName = varNameStr (head (extractVarNames callExpr))

generateArgumentTranslation :: SubroutineTable -> Fortran Anno -> ArgumentTranslation
generateArgumentTranslation subTable (Call anno src callExpr arglist) = varNameReplacements
		where
			subroutineName = varNameStr (head (extractVarNames callExpr))
			subroutineMaybe = DMap.lookup subroutineName subTable
			(subroutineParsed, subroutine) = case subroutineMaybe of
									Nothing -> (False, error "generateArgumentTranslationSubroutines")
									Just sub -> (True, sub)
			(Sub _ _ _ _ arg _) = subroutineTable_ast subroutine

			callArgs = everything (++) (mkQ [] extractExpr_list) arglist
			bodyArgs = everything (++) (mkQ [] extractArgName) arg

			callArgs_varNames = map (\x -> if extractVarNames x == [] then error ("substituteArguments: " ++ (show x)) else  head (extractVarNames x)) callArgs
			bodyArgs_varNames = map (\(ArgName _ str) -> VarName nullAnno str) bodyArgs

			varNameReplacements = foldl (\dmap (old, new) -> DMap.insert old new dmap) DMap.empty (zip bodyArgs_varNames callArgs_varNames)

--	The following 5 functions allow for the use of the argument translation datastructures. These functions actually produce the translated
--	versions of VarNames.
translateArgumentsSubroutine :: ArgumentTranslationSubroutines -> String -> [VarName Anno] -> [VarName Anno]
translateArgumentsSubroutine argTranslations subroutineName args = map (translateArgument argTranslationsSubroutine) args
		where
			argTranslationsSubroutine = getSubroutineArgumentTranslation argTranslations subroutineName

translateArgumentSubroutine :: ArgumentTranslationSubroutines -> String -> VarName Anno -> VarName Anno
translateArgumentSubroutine argTranslations subroutineName arg = translateArgument argTranslationsSubroutine arg
		where
			argTranslationsSubroutine = getSubroutineArgumentTranslation argTranslations subroutineName

getSubroutineArgumentTranslation :: ArgumentTranslationSubroutines -> String -> ArgumentTranslation
getSubroutineArgumentTranslation argTranslation subName = DMap.findWithDefault (DMap.empty) subName argTranslation

--	Taking a subroutine table and a set of kernel pairs (old, new), replace the entries of the 'old' kernels with with their associated 'new' kernels, using the same
--	subroutine names and filenames.
replaceKernels_foldl :: [(Fortran Anno, Fortran Anno)] -> SubroutineTable -> String -> SubroutineTable
replaceKernels_foldl kernelPairs subTable subName = DMap.insert subName (newAst, filename) subTable
		where
			ast = subroutineTable_ast subroutine
			filename = subroutineTable_filename subroutine
			subroutine = DMap.findWithDefault (error "replaceKernels_foldl") subName subTable
			newAst = replaceKernels kernelPairs ast

extractProgUnitName :: ProgUnit Anno -> String
extractProgUnitName ast 	|	subNames == [] = error ((show ast) ++ "\n\nextractProgUnitName: no subNames")
							|	otherwise = extractStringFromSubName (head subNames)
		where
			subNames = everything (++) (mkQ [] getSubNames) ast

extractSubroutines ast = everything (++) (mkQ [] extractSubroutines') ast

extractSubroutines' :: ProgUnit Anno -> [ProgUnit Anno]
extractSubroutines' codeSeg = case codeSeg of
								(Sub _ _ _ _ _ _) -> [codeSeg]
								_ -> []

replaceKernels :: [(Fortran Anno, Fortran Anno)] -> ProgUnit Anno -> ProgUnit Anno
replaceKernels kernelPairs subroutine = foldl (\accumSub (old, optim) -> replaceFortran accumSub old optim) subroutine kernelPairs

extractAllCalls ast = everything (++) (mkQ [] extractCalls) ast

extractCalls codeSeg = case codeSeg of
							Call _ _ _ _ -> [codeSeg]
							_ -> []

extractStringFromSubName :: SubName Anno -> String
extractStringFromSubName (SubName _ str) = str