module VarDependencyAnalysis where

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List

import LanguageFortranTools

--	Type used to colate dependency data between variables within a particular block of code
--							Variable A 			depends on all these variables
type VarDependencyRecord = (VarName [String], 	[VarName [String]])
type VarDependencyAnalysis = [VarDependencyRecord]

analyseDependencies :: Fortran [String] -> VarDependencyAnalysis
analyseDependencies codeSeg = foldl (\accum item -> constructDependencies accum item) [] assignments
						where
							assignments = extractAssigments codeSeg

extractAssigments :: Fortran [String] -> [Fortran [String]]
extractAssigments = everything (++) (mkQ [] extractAssigments')

extractAssigments' :: Fortran [String] -> [Fortran [String]]
extractAssigments' codeSeg = case codeSeg of
								Assg _ _ _ _ -> [codeSeg]
								_	-> []

constructDependencies :: VarDependencyAnalysis -> Fortran [String] -> VarDependencyAnalysis
constructDependencies prevAnalysis (Assg _ _ expr1 expr2) = foldl (\accum item -> addDependencies accum item readVars) prevAnalysis writtenVars
							where
								writtenOperands = extractOperands expr1
								readOperands = extractOperands expr2

								writtenVars = foldl (\accum item -> accum ++ extractVarNames item) [] writtenOperands
								readVars = foldl (\accum item -> accum ++ extractVarNames item) [] readOperands
constructDependencies prevAnalysis _ = prevAnalysis

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