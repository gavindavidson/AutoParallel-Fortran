module ConstantFolding where

import Data.Generics (mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran
import Data.Char
import Data.List
import qualified Data.Map as DMap 
import System.Directory

import LanguageFortranTools

type Constants = DMap.Map (VarName Anno) (Expr Anno)
type FilenameMapping = DMap.Map String (ProgUnit Anno)

foldConstants :: Program Anno -> IO (Program Anno)
foldConstants prog = do
		return prog

buildConstants :: Program Anno -> IO (Constants)
buildConstants prog = do
		let imports = extractImports prog
		filenames <- resolveModuleFilenames
		return DMap.empty

extractImports :: Program Anno -> [String]
extractImports prog = stringUses
	where
		uses = everything (++) (mkQ [] getUses) prog
		stringUses = map (\(Use _ (str, _) _ _) -> str) uses

resolveImport :: FilenameMapping -> String -> String
resolveImport mapping moduleName = moduleName

resolveModuleFilenames :: IO(FilenameMapping)
resolveModuleFilenames = do
				currentDir <- getCurrentDirectory
				filenames <- getDirectoryContents currentDir
				-- let parsedPrograms = parseFiles filenames
				-- let mapping = resolveInternalModules DMap.empty (parsedPrograms)
				return DMap.empty

resolveInternalModules :: FilenameMapping -> [ProgUnit Anno] -> FilenameMapping
resolveInternalModules mapping prog = foldl (\accum item -> DMap.insert (getUnitName item) item accum) mapping prog
