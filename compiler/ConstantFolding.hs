module ConstantFolding where

import Data.Generics (mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran
import Data.Char
import Data.List
import qualified Data.Map as DMap 

import LanguageFortranTools

foldConstants :: Program Anno -> Program Anno
foldConstants prog = prog