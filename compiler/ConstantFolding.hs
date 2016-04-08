module ConstantFolding where

--In future, the compiler will perhaps employ constant folding and an expression evaluator to perform
--more in depth analysis. However, at the moment this functionality will not be included.

import Data.Generics (mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran
import Data.Char
import Data.List
import Control.Monad
import Data.List.Split
import qualified Data.Map as DMap 
import System.Directory

import LanguageFortranTools

type Constants = DMap.Map (VarName Anno) (Expr Anno)
type FilenameMap = DMap.Map String (ProgUnit Anno)

foldConstants filename progUnit = do
		-- constants <- mapM (buildConstants_recursive) prog
		let path = extractPath filename
		constants <- buildConstants_recursive path progUnit
		putStr $ show constants
		return progUnit

buildConstants_recursive :: String -> ProgUnit Anno -> IO (Constants)
buildConstants_recursive path progUnit = do
		availableImports <- gatherAvailableImports path
		let importStrings = extractScopedImports progUnit
		let importedProgunits = map (resolveImport availableImports) importStrings

		recursiveConstants_list <- mapM (buildConstants_recursive path) importedProgunits
		let recursiveConstants = foldl (combineConstants) DMap.empty recursiveConstants_list

		let constants = buildConstants_single recursiveConstants progUnit

		return constants

buildConstants_single :: Constants -> ProgUnit Anno -> Constants
buildConstants_single previousConsts progUnit = constants
		where
			declarations = extractDeclarations progUnit
			declarationExprs = foldl (\accum item -> accum ++ (extractDeclarationExpr item)) [] declarations
			nonNullDeclarationExprs = filter (\(var, expr) -> (applyGeneratedSrcSpans expr) /= NullExpr nullAnno nullSrcSpan) declarationExprs
			constants = foldl (\accum (var, expr) -> DMap.insert var expr accum) previousConsts nonNullDeclarationExprs

extractDeclarationExpr :: Decl Anno-> [(VarName Anno, Expr Anno)]
extractDeclarationExpr (Decl anno src exprList typ) = map (\(expr1, expr2, _) -> (head (extractVarNames expr1), expr2) ) exprList
extractDeclarationExpr _ = []

-- [(Expr p, Expr p, Maybe Int)]

gatherAvailableImports :: String -> IO(FilenameMap)
gatherAvailableImports path = do
		currentDir <- getCurrentDirectory
		currentDirFilenames <- getDirectoryContents currentDir
		sourceDirBasenames <- getDirectoryContents path
		let sourceDirFilenames = map (\x -> path ++ x) sourceDirBasenames
		let filenames = currentDirFilenames ++ sourceDirFilenames
		let fortranFilenames = filter (\x -> (last (splitOn "." x)) == "f95") filenames
		putStr "\n"
		putStr "Fortran filenames:\n"
		putStr $ show $ fortranFilenames
		moduleList <- foldM (resolveModule) DMap.empty fortranFilenames
		return moduleList

extractAllImports :: ProgUnit Anno -> [String]
extractAllImports progUnit = stringUses
	where
		uses = everything (++) (mkQ [] getUses) progUnit
		stringUses = map (\(Use _ (str, _) _ _) -> str) uses

extractScopedImports :: ProgUnit Anno -> [String]
extractScopedImports progUnit = foldl (++) [] (gmapQ (mkQ [] getUsesStrings_recursive) progUnit)

getUsesStrings_recursive :: Uses Anno -> [String]
getUsesStrings_recursive (Use _ (str, _) nestedUses _) = [str] ++ getUsesStrings_recursive nestedUses
getUsesStrings_recursive _ = []

resolveImport :: FilenameMap -> String -> ProgUnit Anno
resolveImport filenameMap moduleName = DMap.findWithDefault (error ("Cannot find imported file " ++ moduleName)) moduleName filenameMap

resolveModule :: FilenameMap -> String -> IO(FilenameMap)
resolveModule filenameMap filename = do
			putStr ("\nProcessing " ++ filename)
			prog <- parseFile filename
			putStr ("\nProcessed " ++ filename)
			let moduleNames = map (getUnitName) prog
			let map = foldl (\accum (name, ast) -> DMap.insert name ast accum) filenameMap (zip moduleNames prog)

			return map

combineConstants :: Constants -> Constants -> Constants
combineConstants constants1 constants2 = foldl (\accum item -> DMap.insert item (DMap.findWithDefault (error "Missing constant") item constants2) accum) constants1 (DMap.keys constants2)

extractDeclarations :: ProgUnit Anno -> [Decl Anno]
extractDeclarations progUnit = everything (++) (mkQ [] getDecl) progUnit

extractPath :: String -> String
extractPath filename = extractPath' (splitOn "/" filename)

extractPath' :: [String] -> String
extractPath' (x:[]) = ""
extractPath' (x:xs) = x ++ "/" ++ extractPath' xs

getDecl :: Decl Anno -> [Decl Anno]
getDecl (DSeq _ _ _) = []
getDecl decl = [decl]