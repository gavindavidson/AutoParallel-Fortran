module VarDependencyAnalysis where

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List
import qualified Data.Map.Strict as DMap

import LanguageFortranTools
import VarAccessAnalysis

--	The code in this file is used to perform some simple dependency analysis for a block of code. A call to 'analyseDependencies'
--	will produce a set of direct dependencies between variables. A direct dependency is formed when one variable is used in the 
--	calculation of another variable's assignment. It is possible under this scheme for variables to depend upon themselves and this
--	fact is ued by Transformer.hs when looking to determine whether or not a loop represnets a reduction.

--	THIS WILL BE CHANGED TO A MAP, RATHER THAN A LIST.
--	Type used to colate dependency data between variables within a particular block of code
--							Variable A 			depends on all these variables
--type VarDependencyRecord = (VarName Anno, 	[VarName Anno])
--type VarDependencyAnalysis = [VarDependencyRecord]
--type VarDependencyAnalysis = DMap.Map (VarName Anno) [VarName Anno]
type VarDependencyAnalysis = DMap.Map (VarName Anno) [Expr Anno]

analyseDependencies :: VarAccessAnalysis -> Fortran Anno -> VarDependencyAnalysis
analyseDependencies accessAnalysis codeSeg = foldl (\accum item -> constructDependencies accessAnalysis accum item) DMap.empty assignments
						where
							assignments = extractAssigments codeSeg

extractAssigments :: Fortran Anno -> [Fortran Anno]
extractAssigments codeSeg = case codeSeg of 
								Assg _ _ _ _ -> [codeSeg]
								_	-> foldl (++) [] (gmapQ (mkQ [] extractAssigments) codeSeg)

constructDependencies :: VarAccessAnalysis -> VarDependencyAnalysis -> Fortran Anno -> VarDependencyAnalysis
constructDependencies accessAnalysis prevAnalysis (Assg _ _ expr1 expr2) = foldl (\accum item -> addDependencies accum item readOperands) prevAnalysis writtenVarNames
							where
								--	As part of Language-Fortran's assignment type, the first expression represents the 
								--	variable being assigned to and the second expression is the thing being assigned
								writtenOperands = extractOperands expr1
								readOperands = extractOperands expr2
								--readDependencies = foldl (\accum item -> if isFunctionCall accessAnalysis item then accum ++ (extractContainedVars item) else accum ++ [item]) [] readOperands
								readDependencies = foldl (\accum item -> accum ++ (extractContainedVars item) ++ [item]) [] readOperands

								writtenVarNames = foldl (\accum item -> accum ++ extractVarNames item) [] writtenOperands
								--readVars = foldl (\accum item -> accum ++ item) [] readDependencies

constructDependencies accessAnalysis prevAnalysis _ = prevAnalysis

--addDependencies :: VarDependencyAnalysis -> VarName Anno -> [VarName Anno] -> VarDependencyAnalysis
--	A dependent depends on a dependee. For example
--		A = B + 12
--	A depends on B. A is the dependee, B is the dependent
addDependencies :: VarDependencyAnalysis -> VarName Anno -> [Expr Anno] -> VarDependencyAnalysis
addDependencies prevAnalysis dependent dependees = foldl (\accum item -> addDependency accum dependent item) prevAnalysis dependees

addDependency :: VarDependencyAnalysis -> VarName Anno -> Expr Anno -> VarDependencyAnalysis
addDependency prevAnalysis dependent dependee = appendToMap dependent dependee prevAnalysis

getDependencies :: VarDependencyAnalysis -> VarName Anno -> [Expr Anno]
getDependencies analysis queryExpr = DMap.findWithDefault [] queryExpr analysis

isDirectlyDependentOn :: VarDependencyAnalysis -> VarName Anno -> Expr Anno -> Bool
isDirectlyDependentOn analysis potDependent potDependee = elem potDependee dependencies
										where
											dependencies = getDependencies analysis potDependent 

isIndirectlyDependentOn:: VarDependencyAnalysis -> VarName Anno -> Expr Anno -> Bool
isIndirectlyDependentOn analysis potDependent potDependee	|	isDirectlyDependentOn analysis potDependent potDependee = True
															|	otherwise = foldl (||) False $ map (\x -> isIndirectlyDependentOn analysis (head $ extractVarNames x) potDependee) dependencies
																	where 
																		dependencies = getDependencies analysis potDependent 