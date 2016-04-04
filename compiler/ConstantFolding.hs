module ConstantFolding where

import Data.Generics (mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran
import Data.Char
import Data.List
import Control.Monad
import qualified Data.Map as DMap 
import System.Directory

import LanguageFortranTools

type Constants = DMap.Map (VarName Anno) (Expr Anno)
type FilenameMap = DMap.Map String (ProgUnit Anno)

foldConstants prog = do
		constants <- mapM (buildConstants_recursive) prog
		return prog

buildConstants_recursive :: ProgUnit Anno -> IO (Constants)
buildConstants_recursive progUnit = do
		availableImports <- gatherAvailableImports
		let importStrings = extractImports progUnit
		let importedProgunits = map (resolveImport availableImports) importStrings
		let localConstants = buildConstants_single DMap.empty progUnit

		return localConstants

buildConstants_single :: Constants -> ProgUnit Anno -> Constants
buildConstants_single prevConstants prog = DMap.empty

gatherAvailableImports :: IO(FilenameMap)
gatherAvailableImports = do
		currentDir <- getCurrentDirectory
		filenames <- getDirectoryContents currentDir
		headParsedFiles <- parseFile (head filenames)
		moduleList <- foldM (resolveModule) DMap.empty filenames
		return moduleList

extractImports :: ProgUnit Anno -> [String]
extractImports prog = stringUses
	where
		uses = everything (++) (mkQ [] getUses) prog
		stringUses = map (\(Use _ (str, _) _ _) -> str) uses

--extractImports :: Program Anno -> [String]
--extractImports prog = stringUses
--	where
--		uses = everything (++) (mkQ [] getUses) prog
--		stringUses = map (\(Use _ (str, _) _ _) -> str) uses

resolveImport :: FilenameMap -> String -> ProgUnit Anno
resolveImport filenameMap moduleName = DMap.findWithDefault (error ("Cannot find imported file " ++ moduleName)) moduleName filenameMap

resolveModule :: FilenameMap -> String -> IO(FilenameMap)
resolveModule filenameMap filename = do
			prog <- parseFile filename
			let moduleNames = map (getUnitName) prog
			let map = foldl (\accum (name, ast) -> DMap.insert name ast accum) filenameMap (zip moduleNames prog)

			return map

--resolveModule :: String -> IO(FilenameMap)
--resolveModule filename = do
--			prog <- parseFile filename
--			let moduleNames = map (getUnitName) prog
--			let map = foldl (\accum (name, ast) -> DMap.insert name ast accum) DMap.empty (zip moduleNames prog)

--			return map