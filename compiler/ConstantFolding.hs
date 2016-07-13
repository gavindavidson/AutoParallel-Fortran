module ConstantFolding 				(foldConstants)

where

import Data.Generics 				(mkQ, mkT, gmapQ, gmapT, everything, everywhere)
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
foldConstants codeSeg = replaceVarsWithConstants codeSeg constants_assgs
		where
			--	First step is to collect all of the declarations in the program unit before using those declarations to begin
			--	building up a table of constants.
			decls = everything (++) (mkQ [] extractDecl) codeSeg
			constants_decls = addDeclsToConstants decls DMap.empty

			--	Second, any variables that are only assigned a value once may be considered constants and thus added to the 
			--	constants table. However, these assignments must appear at the top level of scope in the program, and not
			--	in loops. The approach here is to find a list of variables that are only assigned at the top level and remove
			--	any of those variables that already exist in the constant table (ie, already have been given a value). This list
			--	is then a list of 'allowed' variables that are constants. The next step is to traverse the body of the program
			--	to find values for these allowed constants.
			varAssignments = extractTopLevelAssignments codeSeg
			assigneeVarNames = map extractAssigneeVarName varAssignments
			allowedAssignments = listSubtract (listExtractSingleAppearances assigneeVarNames) (map (\x -> VarName nullAnno x) (DMap.keys constants_decls))
			constants_assgs = addAssignmentsToConstants varAssignments allowedAssignments constants_decls constants_decls



addDeclsToConstants :: [Decl Anno] -> ValueTable -> ValueTable
addDeclsToConstants ((Decl _ _ lst typ):followingDecls) constants = addDeclsToConstants followingDecls newConstants
		where
			--	The format of the declarations is a list of tuples where the first two elements make up an assignment statement. VarNames of the variables
			--	that are assigned to (assignees) are gathered and then the null assignments are weeded out (a null assignment is when a variable is
			--	declared and not given a value). The right hand side of the assignments is then evaluated, if possible, using the constants gathered so far
			--	and the variables that can be evaluated are added to the constants table.
			assignments = map (\(assignee, assignment, _) -> (head $ extractVarNames assignee, assignment)) lst
			nonNullAssignments = filter (\(assignee, assignment) -> nonNullExprs_filter assignment) assignments
			evaluatedAssignments_maybe = map (\(assignee, assignment) -> (assignee, (evaluateExpr constants assignment))) nonNullAssignments
			evaluatedAssignments = declsFilterNothingEvaluations evaluatedAssignments_maybe
			newConstants = foldl (\accum (assignee, assignment) -> addToValueTable_type assignee assignment (extractBaseType typ) accum) constants evaluatedAssignments
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

declsFilterNothingEvaluations :: [(VarName Anno, Maybe (Float))] -> [(VarName Anno, Float)]
declsFilterNothingEvaluations ((assignee, assignment):lst) = case assignment of
														Nothing -> declsFilterNothingEvaluations lst
														Just a -> [(assignee, a)] ++ declsFilterNothingEvaluations lst
declsFilterNothingEvaluations [] = []

replaceVarsWithConstants :: ProgUnit Anno -> ValueTable -> ProgUnit Anno
replaceVarsWithConstants codeSeg constants = everywhere (mkT (replaceVarsWithConstants_fortran constants)) codeSeg

--	All appearences of a variable that appears in the constant table are replaced with a 'Con _ _ _' node OTHER THAN when those variables
--	appear on the left side of an assignment operations
replaceVarsWithConstants_fortran :: ValueTable -> Fortran Anno -> Fortran Anno
replaceVarsWithConstants_fortran constants (Assg src anno expr1 expr2) = Assg src anno (replaceArrayAccessesWithConstants_expr constants expr1) (replaceVarsWithConstants_expr constants expr2)
replaceVarsWithConstants_fortran constants codeSeg = gmapT (mkT (replaceVarsWithConstants_expr constants)) codeSeg

replaceVarsWithConstants_expr :: ValueTable -> Expr Anno ->  Expr Anno
replaceVarsWithConstants_expr constants expr = case expr of
												Var _ _ _ -> transformed
												_ -> gmapT (mkT (replaceVarsWithConstants_expr constants)) expr
		where
			varName_str = varNameStr $ head $ extractVarNames expr
			lookup = lookupValueTable varName_str constants
			transformed = case lookup of
							Nothing -> replaceArrayAccessesWithConstants_expr constants expr
							Just val -> generateFloatConstant val

replaceArrayAccessesWithConstants_expr :: ValueTable -> Expr Anno -> Expr Anno
replaceArrayAccessesWithConstants_expr constants (Var anno src lst) = Var anno src (map (replaceArrayAccessesWithConstants_varExprList constants) lst)

replaceArrayAccessesWithConstants_varExprList :: ValueTable -> (VarName Anno, [Expr Anno]) -> (VarName Anno, [Expr Anno])
replaceArrayAccessesWithConstants_varExprList constants (var, exprList) = (var, map (replaceVarsWithConstants_expr constants) exprList)