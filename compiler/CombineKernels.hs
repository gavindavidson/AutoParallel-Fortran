module CombineKernels where

--	This file contains code that handles combining adjacent and nested kernels. The intention is that the top level 'combineKernels' function will be called
--	against an AST that has already been transformed with parallel kernels applied. When calling the 'combineKernels' function, a Maybe(Float) is supplied
--	that represents a the loop fusion bound (the limit of how different the end values for two loops can be for them to be fused). If this input is 'Nothing'
--	then no bound is enforced and all loops that meet the other fusion criteria are fused.

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran

import LanguageFortranTools
import VarDependencyAnalysis


combineKernels :: Maybe(Float) -> Program Anno -> Program Anno
combineKernels bound codeSeg = map (everywhere (mkT (combineKernelsBlock bound))) codeSeg

combineKernelsBlock :: Maybe(Float) -> Block Anno -> Block Anno
combineKernelsBlock bound block = combinedAdjacentNested
				where
					combinedNested = everywhere (mkT (combineNestedKernels)) block
					combinedAdjacentNested = everywhere (mkT (combineAdjacentKernels bound)) combinedNested

--	In the case where a kernel only contains another kernel of the same type, the two can be combined into one larger kernel.
--	The nested kernel is definitely compatible with the outer kernel as this check has been performed by Transformer.hs. This function
--	essentially mops up the representation to make code generation a little easier.
combineNestedKernels :: Fortran Anno -> Fortran Anno
combineNestedKernels codeSeg = case codeSeg of
					(OpenCLMap anno1 src1 outerReads outerWrites outerLoopVs fortran) -> case fortran of
								(OpenCLMap anno2 src2 innerReads innerWrites innerLoopVs innerFortran) -> 
										newCodeSeg
											where 
												newCodeSeg = appendAnnotation (OpenCLMap (combinedAnnotations) src1 reads writes loopVs innerFortran) newAnnotation ""
												reads = innerReads
												writes = innerWrites
												loopVs = listRemoveDuplications $ outerLoopVs ++ innerLoopVs
												newAnnotation = compilerName ++ ": Nested map at " ++ errorLocationFormatting src2 ++ " fused into surrounding map"
												combinedAnnotations = combineAnnotations anno1 anno2

								otherwise -> codeSeg

					(OpenCLReduce anno1 src1 outerReads outerWrites outerLoopVs outerRedVs fortran) -> case fortran of
								(OpenCLReduce anno2 src2 innerReads innerWrites innerLoopVs innerRedVs innerFortran) -> 
										newCodeSeg
											where 
												newCodeSeg = appendAnnotation (OpenCLReduce (combinedAnnotations) src1 reads writes loopVs redVs innerFortran) newAnnotation ""
												reads = innerReads
												writes = innerWrites
												loopVs = listRemoveDuplications $ outerLoopVs ++ innerLoopVs
												redVs = innerRedVs
												newAnnotation = compilerName ++ ": Nested reduction at " ++ errorLocationFormatting src2 ++ " fused into surrounding reduction"
												combinedAnnotations = combineAnnotations anno1 anno2

								otherwise -> codeSeg
					otherwise -> codeSeg

--	In the case that two maps are adjacent to one and other and their iterator conditions are compatible, the kernels can be combined.
combineAdjacentKernels :: Maybe(Float) -> Fortran Anno -> Fortran Anno
combineAdjacentKernels bound codeSeg = case codeSeg of
					(FSeq anno1 src1 fortran1 (FSeq anno2 _ fortran2 fortran3)) -> case fortran1 of
							OpenCLMap _ src2 _ _ _ _ -> case fortran2 of
									OpenCLMap _ src3 _ _ _ _ -> case attemptCombineAdjacentMaps bound fortran1 fortran2 of
																Just oclmap -> appendAnnotation (FSeq (combinedAnnotations) src1 oclmap fortran3) newAnnotation "" 

																	where
																		newAnnotation = compilerName ++ ": Adjacent maps at " ++ errorLocationFormatting src2 ++ " and " ++ errorLocationFormatting src3 ++ " fused"
																		combinedAnnotations = combineAnnotations anno1 anno2
																Nothing -> codeSeg
									otherwise	-> codeSeg
							otherwise	-> codeSeg
					(FSeq anno1 src1 fortran1 fortran2) -> case fortran1 of
							OpenCLMap _ src2 _ _ _ _ -> case fortran2 of
									OpenCLMap _ src3 _ _ _ _ -> case attemptCombineAdjacentMaps bound fortran1 fortran2 of
																Just oclmap -> appendAnnotation (appendAnnotationMap oclmap anno1) newAnnotation "" -- FSeq (anno1 ++ anno2 ++ [newAnnotation]) src1 oclmap fortran3  
																	where
																		newAnnotation = compilerName ++ ": Adjacent maps at " ++ errorLocationFormatting src2 ++ " and " ++ errorLocationFormatting src3 ++ " fused"
																Nothing -> codeSeg
									otherwise	-> codeSeg
							otherwise	-> codeSeg
					otherwise -> codeSeg

--	This function constructs the combined adjacent maps. In the case that the iterator conditions differ slightly, it adds conditional constructs around appropriate
--	parts of the body of the kernels.
attemptCombineAdjacentMaps :: Maybe(Float) -> Fortran Anno -> Fortran Anno -> Maybe(Fortran Anno)
attemptCombineAdjacentMaps 	bound
							(OpenCLMap anno1 src1 reads1 writes1 loopVs1 fortran1) 
							(OpenCLMap anno2 src2 reads2 writes2 loopVs2 fortran2) 	| resultLoopVars == [] -- || loopDependencyBool  
																											= Nothing
																					| otherwise = Just(resultantMap)
									where
										newSrc = generateSrcSpanMerge src1 src2
										combinedReads = listRemoveDuplications $ reads1 ++ reads2
										writes = listRemoveDuplications $ writes1 ++ writes2
										anno = combineAnnotations anno1 anno2

										fortran = appendFortran_recursive 	(if yPredicateList /= [] then (generateIf yAndPredicate fortran2) else fortran2) 
																			(if xPredicateList /= [] then (generateIf xAndPredicate fortran1) else fortran1) 

										combinedConditions = loopVariableCombinationConditions bound loopVs1 loopVs2
										(xPredicateList, yPredicateList, resultLoopVars) = case combinedConditions of
											Just conditions -> conditions
											Nothing -> ([],[],[])

										xAndPredicate = generateAndExprFromList xPredicateList
										yAndPredicate = generateAndExprFromList yPredicateList

										loopVs = listRemoveDuplications $ loopVs1 ++ loopVs2
										(loopDependencyBool, _, _) = loopCarriedDependencyCheck resultantMap

										resultantMap = OpenCLMap anno newSrc combinedReads writes resultLoopVars fortran

--	Recursive function to check sets of loop iterators against each other for compatibility. An OpenCLMap or OpenCLReduce can have more than one loop
--	iterator defined because they can represent nested loops. The function checks that the iterator conditions in each object match, IN ORDER. For example,
--	for i:10, j:20, k:30 would match to for i:10, j:20, k:30 but not to for j:20,i:10,k:30
--	Handles loops with slightly different end points but not loop iterators having different names.
loopVariableCombinationConditions :: Maybe(Float) 
									-> [(VarName Anno, Expr Anno, Expr Anno, Expr Anno)] 
									-> [(VarName Anno, Expr Anno, Expr Anno, Expr Anno)] 
									-> Maybe([Expr Anno], [Expr Anno], [(VarName Anno, Expr Anno, Expr Anno, Expr Anno)])
loopVariableCombinationConditions 	bound
									((xVarName, xStart, xEnd, xStep):xs) 
									((yVarName, yStart, yEnd, yStep):ys) 	|	sameVarNames && sameStart && sameStep && sameEnd && nextCombines = Just(xNext,yNext, [loopVars] ++ loopVarsNext)
																			|	sameVarNames && sameStart && sameStep && endLinearFunction && nextCombines = Just(predicatListX ++ xNext, predicatListY ++ yNext, [loopVars] ++ loopVarsNext)
																			|	otherwise = Nothing
										where
											sameVarNames = xVarName == yVarName
											sameStart = (everywhere (mkT standardiseSrcSpan) xStart) == (everywhere (mkT standardiseSrcSpan) yStart)
											sameStep = (everywhere (mkT standardiseSrcSpan) xStep) == (everywhere (mkT standardiseSrcSpan) yStep)
											sameEnd = (everywhere (mkT standardiseSrcSpan) xEnd) == (everywhere (mkT standardiseSrcSpan) yEnd)

											xAddsToY = isConstantAdditionOf bound xEnd yEnd
											yAddsToX = isConstantAdditionOf bound yEnd xEnd
											xTakesFromY = isConstantSubtractionOf bound xEnd yEnd
											yTakesFromX = isConstantSubtractionOf bound yEnd xEnd
											endLinearFunction = xAddsToY || yAddsToX || xTakesFromY || yTakesFromX

											predicatListX = (if yAddsToX || xTakesFromY then [generateLTExpr (generateVar xVarName) xEnd]
															 else [])
											predicatListY = (if xAddsToY || yTakesFromX then [generateLTExpr (generateVar yVarName) yEnd]
															 else [])
											loopVars = (if yAddsToX || xTakesFromY then (yVarName, yStart, yEnd, yStep)
															 else (xVarName, xStart, xEnd, xStep))

											nextCombines = case next of
													Just a -> True
													Nothing -> False

											next = loopVariableCombinationConditions bound xs ys
											xNext = case next of
												Just a -> (\(x, _, _) -> x) a
												Nothing -> []
											yNext = case next of
												Just a -> (\(_, y, _) -> y) a
												Nothing -> []
											loopVarsNext = case next of
												Just a -> (\(_, _, z) -> z) a
												Nothing -> []

loopVariableCombinationConditions _ [] [] = Just([],[],[])
loopVariableCombinationConditions _ _ _ = Nothing

--	The following functions are used to determine whether slightly differing loop conditions are compatible. If the end point of one is a constant
--	addition or subtraction of the other then the loops can be fused.
isConstantAdditionOf :: Maybe(Float) -> Expr Anno -> Expr Anno -> Bool
isConstantAdditionOf bound (Bin anno2 src2 op expr1 expr2) var 	|	exprContainsVar && expr2Check = case op of
																								Plus _ -> True
																								otherwise -> False
																|	otherwise = False
																	where 
																		standardVar = (everywhere (mkT standardiseSrcSpan) var)
																		standardExpr1 = (everywhere (mkT standardiseSrcSpan) expr1)
																		exprContainsVar = standardVar == standardExpr1
																		expr2Check = checkConstantLessThan expr2 bound
isConstantAdditionOf _ _ _ = False

isConstantSubtractionOf :: Maybe(Float) -> Expr Anno -> Expr Anno -> Bool
isConstantSubtractionOf bound (Bin anno2 src2 op expr1 expr2) var 	|	exprContainsVar && expr2Check = case op of
																								Minus _ -> True
																								otherwise -> False
																	|	otherwise = False
																		where 
																			standardVar = (everywhere (mkT standardiseSrcSpan) var)
																			standardExpr1 = (everywhere (mkT standardiseSrcSpan) expr1)
																			exprContainsVar = standardVar == standardExpr1
																			expr2Check = checkConstantLessThan expr2 bound
isConstantSubtractionOf _ _ _ = False

checkConstantLessThan :: Expr Anno -> Maybe(Float) -> Bool
checkConstantLessThan expr (Just bound) = exprCheck
			where
				exprCons = case expr of
						Con _ _ str -> Just (read str :: Float)
						_	-> Nothing
				exprCheck = case exprCons of
						Just number -> number <= bound
						Nothing -> False
checkConstantLessThan expr Nothing = case expr of
						Con _ _ str -> True
						_	-> False