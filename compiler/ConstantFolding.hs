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
foldConstants codeSeg = replaceVarsWithConstants codeSeg constants_assgs-- error $ show constants_assgs -- codeSeg
		where
			decls = everything (++) (mkQ [] extractDecl) codeSeg
			constants_decls = addDeclsToConstants decls DMap.empty

			varAssignments = extractTopLevelAssignments codeSeg-- everything (++) (mkQ [] extractAssignments) codeSeg
			assigneeVarNames = map extractAssigneeVarName varAssignments
			allowedAssignments = listSubtract (listExtractSingleAppearances assigneeVarNames) (map (\x -> VarName nullAnno x) (DMap.keys constants_decls))
			constants_assgs = addAssignmentsToConstants varAssignments allowedAssignments constants_decls constants_decls



addDeclsToConstants :: [Decl Anno] -> ValueTable -> ValueTable
addDeclsToConstants ((Decl _ _ lst typ):followingDecls) constants = addDeclsToConstants followingDecls newConstants
		where
			assignments = map (\(assignee, assignment, _) -> (head $ extractVarNames assignee, assignment)) lst
			nonNullAssignments = filter (\(assignee, assignment) -> nonNullExprs_filter assignment) assignments
			evaluatedAssignments = map (\(assignee, assignment) -> (assignee, fromMaybe 0 (evaluateExpr constants assignment))) nonNullAssignments
			newConstants = foldl (\accum (assignee, assignment) -> addToValueTable_type assignee assignment (extractBaseType typ) accum) constants evaluatedAssignments
			-- newConstants = foldl (\accum (assignee, assignment) -> DMap.insert assignee (assignment, Real nullAnno) accum) constants evaluatedAssignments
addDeclsToConstants [] constants = constants
addDeclsToConstants (x:xs) constants =  addDeclsToConstants xs constants

addAssignmentsToConstants :: [Fortran Anno] -> [VarName Anno] -> ValueTable -> ValueTable -> ValueTable
addAssignmentsToConstants ((Assg _ _ expr1 expr2):followingAssgs) allowedAssignments valTable constants 	|	evaluated_bool && onlyAssignment = addAssignmentsToConstants followingAssgs allowedAssignments newValTable newConstants_added
																											|	not onlyAssignment = addAssignmentsToConstants followingAssgs allowedAssignments newValTable newConstants_deleted
																											| 	otherwise = addAssignmentsToConstants followingAssgs allowedAssignments newValTable constants
		where
			assigneeVarName = head $ extractVarNames expr1
			evaluated_maybe = evaluateExpr_type valTable expr2
			(evaluated_bool, evaluated_value, evaluated_type) = case evaluated_maybe of
								Nothing -> (False, 0.0, Real nullAnno)
								Just (val, typ) -> (True, val, typ)

			onlyAssignment = (elem assigneeVarName allowedAssignments)

			newConstants_added = addToValueTable_type assigneeVarName evaluated_value evaluated_type constants
			newConstants_deleted = deleteValueFromTable assigneeVarName constants

			newValTable = if evaluated_bool then addToValueTable assigneeVarName evaluated_value valTable else valTable
addAssignmentsToConstants [] _ valTable constants = constants
addAssignmentsToConstants (x:xs) allowedAssignments valTable constants =  addAssignmentsToConstants xs allowedAssignments valTable constants

extractAssigneeVarName :: Fortran Anno -> VarName Anno
extractAssigneeVarName (Assg _ _ expr _) =  head $ extractVarNames expr
extractAssigneeVarName _ = error "extractAssigneeVarName: Must be used with \"Assg _ _ _ _\" nodes"

-- ProgUnit Anno
extractTopLevelAssignments :: ProgUnit Anno -> [Fortran Anno]
extractTopLevelAssignments codeSeg = filter (\x -> not (elem (extractAssigneeVarName x) forLoopAssgs_varNames)) topLevelAssgs
		where
			(topLevelAssgs, forLoopAssgs) = extractScopedAssignments firstFortran
			forLoopAssgs_varNames = map (extractAssigneeVarName) forLoopAssgs
			firstFortran = head (everything (++) (mkQ [] extractFortran) codeSeg)

extractScopedAssignments :: Fortran Anno -> ([Fortran Anno],[Fortran Anno])
extractScopedAssignments codeSeg = case codeSeg of 
										For _ _ _ _ _ _ _ -> ([], everything (++) (mkQ [] extractAssignments) codeSeg)
										Assg _ _ _ _ -> ([codeSeg],[])
										_ -> foldl (\(a1,b1) (a2,b2) -> (a1++a2,b1++b2)) ([],[]) (gmapQ (mkQ ([],[]) extractScopedAssignments) codeSeg)

nonNullExprs_filter :: Expr Anno -> Bool
nonNullExprs_filter (NullExpr _ _) = False
nonNullExprs_filter _ = True

replaceVarsWithConstants :: ProgUnit Anno -> ValueTable -> ProgUnit Anno
replaceVarsWithConstants codeSeg constants = everywhere (mkT (replaceVarsWithConstants_fortran constants)) codeSeg

replaceVarsWithConstants_fortran :: ValueTable -> Fortran Anno -> Fortran Anno
replaceVarsWithConstants_fortran constants (Assg src anno expr1 expr2) = Assg src anno expr1 (replaceVarsWithConstants_expr constants expr2)
replaceVarsWithConstants_fortran constants codeSeg = gmapT (mkT (replaceVarsWithConstants_expr constants)) codeSeg

replaceVarsWithConstants_expr :: ValueTable -> Expr Anno ->  Expr Anno
replaceVarsWithConstants_expr constants expr = case expr of
												Var _ _ _ -> transformed
												_ -> gmapT (mkT (replaceVarsWithConstants_expr constants)) expr
		where
			varName_str = varNameStr $ head $ extractVarNames expr
			lookup = lookupValueTable varName_str constants
			transformed = case lookup of
							Nothing -> expr
							Just val -> generateFloatConstant val
