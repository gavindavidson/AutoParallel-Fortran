module ConstantFolding where

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

foldConstants progUnit = do
		-- constants <- mapM (buildConstants_recursive) prog
		constants <- buildConstants_recursive progUnit
		return progUnit

buildConstants_recursive :: ProgUnit Anno -> IO (Constants)
buildConstants_recursive progUnit = do
		availableImports <- gatherAvailableImports
		let importStrings = extractScopedImports progUnit
		let importedProgunits = map (resolveImport availableImports) importStrings

		recursiveConstants_list <- mapM (buildConstants_recursive) importedProgunits
		let recursiveConstants = foldl (combineConstants) DMap.empty recursiveConstants_list

		let constants = buildConstants_single recursiveConstants progUnit

		return constants

buildConstants_single :: Constants -> ProgUnit Anno -> Constants
buildConstants_single previousConsts progUnit = DMap.empty
		where
			declartions = extractDeclartions progUnit

gatherAvailableImports :: IO(FilenameMap)
gatherAvailableImports = do
		currentDir <- getCurrentDirectory
		filenames <- getDirectoryContents currentDir
		let fortranFilenames = filter (\x -> (last (splitOn "/" x)) == ".f95") filenames
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
			prog <- parseFile filename
			let moduleNames = map (getUnitName) prog
			let map = foldl (\accum (name, ast) -> DMap.insert name ast accum) filenameMap (zip moduleNames prog)

			return map

combineConstants :: Constants -> Constants -> Constants
combineConstants constants1 constants2 = foldl (\accum item -> DMap.insert item (DMap.findWithDefault (error "Missing constant") item constants2) accum) constants1 (DMap.keys constants2)

extractDeclartions :: ProgUnit Anno -> [Decl Anno]
extractDeclartions progUnit = everything (++) (mkQ [] getDecl) progUnit

getDecl :: Decl Anno -> [Decl Anno]
getDecl (DSeq _ _ _) = []
getDecl decl = [decl]