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