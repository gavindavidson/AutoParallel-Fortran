module VarDependencyAnalysis where

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List

import LanguageFortranTools
import VarAccessAnalysis

--	The code in this file is used to perform some simple dependency analysis for a block of code. A call to 'analyseDependencies'
--	will produce a set of direct dependencies between variables. A direct dependency is formed when one variable is used in the 
--	calculation of another variable's assignment. It is possible under this scheme for variables to depend upon themselves and this
--	fact is ued by Transformer.hs when looking to determine whether or not a loop represnets a reduction.

--	THIS WILL BE CHANGED TO A MAP, RATHER THAN A LIST.
--	Type used to colate dependency data between variables within a particular block of code
--							Variable A 			depends on all these variables
type VarDependencyRecord = (VarName [String], 	[VarName [String]])
type VarDependencyAnalysis = [VarDependencyRecord]

analyseDependencies :: VarAccessAnalysis -> Fortran [String] -> VarDependencyAnalysis
analyseDependencies accessAnalysis codeSeg = foldl (\accum item -> constructDependencies accessAnalysis accum item) [] assignments
						where
							assignments = extractAssigments codeSeg

extractAssigments :: Fortran [String] -> [Fortran [String]]
extractAssigments = everything (++) (mkQ [] extractAssigments')

extractAssigments' :: Fortran [String] -> [Fortran [String]]
extractAssigments' codeSeg = case codeSeg of
								Assg _ _ _ _ -> [codeSeg]
								_	-> []

constructDependencies :: VarAccessAnalysis -> VarDependencyAnalysis -> Fortran [String] -> VarDependencyAnalysis
constructDependencies accessAnalysis prevAnalysis (Assg _ _ expr1 expr2) = foldl (\accum item -> addDependencies accum item readVars) prevAnalysis writtenVars
							where
								--	As part of Language-Fortran's assignment type, the first expression represents the 
								--	variable being assigned to and the second expression is the thing being assigned
								writtenOperands = extractOperands expr1
								--readOperands = case fnCall of
								--			True ->	extractContainedVars expr2
								--			False -> extractOperands expr2
								--fnCall = isFunctionCall accessAnalysis expr2
								readOperands = extractOperands expr2
								readDependencies = foldl (\accum item -> if isFunctionCall accessAnalysis item then accum ++ (extractContainedVars item) else accum ++ [item]) [] readOperands

								writtenVars = foldl (\accum item -> accum ++ extractVarNames item) [] writtenOperands
								--readVars = foldl (\accum item -> accum ++ extractVarNames item) [] readOperands
								readVars = foldl (\accum item -> accum ++ extractVarNames item) [] readDependencies

constructDependencies accessAnalysis prevAnalysis _ = prevAnalysis

addDependencies :: VarDependencyAnalysis -> VarName [String] -> [VarName [String]] -> VarDependencyAnalysis
addDependencies prevAnalysis dependent dependees = foldl (\accum item -> addDependency accum dependent item) prevAnalysis dependees

addDependency :: VarDependencyAnalysis -> VarName [String] -> VarName [String] -> VarDependencyAnalysis
addDependency ((dependent_prev, dependeeList):xs) dependent dependee 	|	dependent_prev == dependent =	[(dependent_prev, (if not (elem dependee dependeeList) then dependeeList ++ [dependee] else dependeeList))] ++ xs
																		|	otherwise =	[(dependent_prev, dependeeList)] ++ addDependency xs dependent dependee
addDependency [] dependent dependee 	= [(dependent, [dependee])]

getDependencies :: VarDependencyAnalysis -> VarName [String] -> [VarName [String]]
getDependencies ((dependent, dependeeList):xs) queryVarname 	|	queryVarname == dependent = dependeeList
																|	otherwise = getDependencies xs queryVarname
getDependencies [] queryVarname	= []

isDirectlyDependentOn :: VarDependencyAnalysis -> VarName [String] -> VarName [String] -> Bool
isDirectlyDependentOn analysis potDependent potDependee = elem potDependee dependencies
										where
											dependencies = getDependencies analysis potDependent 

isIndirectlyDependentOn:: VarDependencyAnalysis -> VarName [String] -> VarName [String] -> Bool
isIndirectlyDependentOn analysis potDependent potDependee	|	isDirectlyDependentOn analysis potDependent potDependee = True
															|	otherwise = foldl (||) False $ map (\x -> isIndirectlyDependentOn analysis x potDependee) dependencies
																	where 
																		dependencies = getDependencies analysis potDependent 