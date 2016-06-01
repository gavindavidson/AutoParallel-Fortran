module ConstantFolding where

import Data.Generics (mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as DMap

import LanguageFortranTools

-- type Constants = DMap.Map (VarName Anno) (Expr Anno)

-- 	STRATEGY
--	+	Extract all declarations. These appear at the start of the program. Use these declarations to begin building constants table,
--		where possible
--	+	Traverse the AST of the supplied progunit and continue to build up constant table. In the simplest case, only variables that
--		are only ever assigned ONE value will be included in the table, therefore variables that are reassigned will be removed.
--	+	Traverse the AST once more and replace any instance of a 'Var _ _ _' found in the constant table with the appropriate
--		'Con _ _ _' node.
--	+	Return the transformed AST.

foldConstants :: ProgUnit Anno -> ProgUnit Anno
foldConstants codeSeg = error $ show constants_decls -- codeSeg
		where
			decls = everything (++) (mkQ [] extractDecl) codeSeg
			constants_decls = addDeclsToConstants decls DMap.empty

			varAssignments = everything (++) (mkQ [] extractAssignments) codeSeg

addDeclsToConstants :: [Decl Anno] -> ValueTable -> ValueTable
addDeclsToConstants ((Decl _ _ lst _):followingDecls) constants = addDeclsToConstants followingDecls newConstants
		where
			assignments = map (\(assignee, assignment, _) -> (varnameStr $ head $ extractVarNames assignee, assignment)) lst
			constantAssignments = filter (\(assignee, assignment) -> constantAssignments_filter assignment) assignments
			evaluatedAssignments = map (\(assignee, assignment) -> (assignee, fromMaybe 0 (evaluateExpr constants assignment))) constantAssignments
			newConstants = foldl (\accum (assignee, assignment) -> DMap.insert assignee assignment accum) constants evaluatedAssignments
addDeclsToConstants [] constants = constants
addDeclsToConstants (x:xs) constants =  addDeclsToConstants xs constants

-- addAssignmentsToConstants :: [Fortran Anno] -> ValueTable -> ValueTable
-- addAssignmentsToConstants ((Assg _ _ expr1 expr2):followingAssgs) constants = 
-- 		where


constantAssignments_filter :: Expr Anno -> Bool
constantAssignments_filter (Con _ _ _) = True
constantAssignments_filter _ = False