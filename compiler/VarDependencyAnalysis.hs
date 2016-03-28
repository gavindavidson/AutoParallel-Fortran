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
							-- assignments = extractAssigments codeSeg
							assignments = everything (++) (mkQ [] extractAssigments) codeSeg

extractAssigments :: Fortran Anno -> [Fortran Anno]
extractAssigments codeSeg = case codeSeg of 
								Assg _ _ _ _ -> [codeSeg]
								_	-> []

-- extractAssigments :: Fortran Anno -> [Fortran Anno]
-- extractAssigments codeSeg = case codeSeg of 
-- 								Assg _ _ _ _ -> [codeSeg]
-- 								_	-> foldl (++) [] (gmapQ (mkQ [] extractAssigments) codeSeg)

constructDependencies :: VarAccessAnalysis -> VarDependencyAnalysis -> Fortran Anno -> VarDependencyAnalysis
constructDependencies accessAnalysis prevAnalysis (Assg _ _ expr1 expr2) = foldl (\accum item -> addDependencies accum item readOperands) prevAnalysis writtenVarNames
							where
								--	As part of Language-Fortran's assignment type, the first expression represents the 
								--	variable being assigned to and the second expression is the thing being assigned
								writtenOperands = filter (isVar) (extractOperands expr1)
								readOperands = filter (isVar) (extractOperands expr2)
								--readDependencies = foldl (\accum item -> if isFunctionCall accessAnalysis item then accum ++ (extractContainedVars item) else accum ++ [item]) [] readOperands
								-- readDependencies = foldl (\accum item -> accum ++ (extractContainedVars item) ++ [item]) [] readOperands

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

getDirectDependencies :: VarDependencyAnalysis -> VarName Anno -> [Expr Anno]
getDirectDependencies analysis queryVarname = DMap.findWithDefault [] queryVarname analysis

getIndirectDependencies :: VarDependencyAnalysis -> VarName Anno -> [Expr Anno]
getIndirectDependencies analysis queryVarname = getIndirectDependencies' analysis queryVarname []

getIndirectDependencies' :: VarDependencyAnalysis -> VarName Anno -> [Expr Anno] -> [Expr Anno]
getIndirectDependencies' analysis queryVarname previouslyProcessed = foldl (\accum item -> accum ++ getIndirectDependencies' analysis (head $ extractVarNames item)  newProcessed) newDependencies newDependencies
											where
												newDependencies = listSubtract (getDirectDependencies analysis queryVarname) previouslyProcessed
												newProcessed = newDependencies ++ previouslyProcessed

isDirectlyDependentOn :: VarDependencyAnalysis -> VarName Anno -> Expr Anno -> Bool
isDirectlyDependentOn analysis potDependent potDependee = elem potDependee dependencies
										where
											dependencies = getDirectDependencies analysis potDependent 

isIndirectlyDependentOn' :: VarDependencyAnalysis -> VarName Anno -> Expr Anno -> [Expr Anno] -> Bool
isIndirectlyDependentOn' analysis potDependent potDependee previouslyProcessed 	|	isDirectlyDependentOn analysis potDependent potDependee = True
																				|	otherwise = foldl (||) False $ map (\x -> isIndirectlyDependentOn' analysis (head $ extractVarNames x) potDependee newProcessed) newDependencies
																						where
																							newDependencies = listSubtract (getDirectDependencies analysis potDependent) previouslyProcessed
																							newProcessed = newDependencies ++ previouslyProcessed

isIndirectlyDependentOn :: VarDependencyAnalysis -> VarName Anno -> Expr Anno -> Bool
isIndirectlyDependentOn analysis potDependent potDependee	|	isDirectlyDependentOn analysis potDependent potDependee = True
															-- |	otherwise = foldl (||) False $ map (\x -> isIndirectlyDependentOn analysis (head $ extractVarNames x) potDependee) dependencies
															|	otherwise = foldl (||) False $ map (\x -> isIndirectlyDependentOn' analysis (head $ extractVarNames x) potDependee []) dependencies

																	where 
																		dependencies = getDirectDependencies analysis potDependent 


loopCarriedDependencyCheck :: [VarName Anno] -> VarDependencyAnalysis -> Expr Anno -> [Expr Anno]
loopCarriedDependencyCheck loopIterators dependencyAnalysis expr = loopCarriedDependencyProof -- foldl (++) [] exprLoopIteratorUsage
								where
									exprLoopIteratorUsage = loopIteratorUsage loopIterators expr
									dependencies = getIndirectDependencies dependencyAnalysis (head $ extractVarNames expr)
									selfDependencies = filter (\item -> listIntersection (extractVarNames item) (extractVarNames expr) /= []) dependencies

									loopCarriedDependencyProof = filter (\item -> exprLoopIteratorUsage /= loopIteratorUsage loopIterators item) selfDependencies

loopIteratorUsage :: [VarName Anno] -> Expr Anno -> [[Expr Anno]]
loopIteratorUsage loopIterators expr = loopIteratorUsageList
			where
				accessExprs = map (extractOperands) (extractContainedVars expr)
				loopIteratorUsageList = map (\item -> if (varNameUsageCheck item loopIterators) then map (applyGeneratedSrcSpans) item else []) accessExprs

varNameUsageCheck :: [Expr Anno] -> [VarName Anno] -> Bool
varNameUsageCheck exprs varnames =  match
			where
				usedVarnames = foldl (\accum item -> accum ++ extractUsedVarName item) [] exprs
				match = (listIntersection varnames usedVarnames) /= []