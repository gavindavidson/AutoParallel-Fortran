module FortranGenerator

where

-- This module houses most of the functions used by the code emission stage of compilation that aim to produce 
-- some new fortran node. While the import heirarchy here is not fantastic, and there is a large degree of coupling,
-- separating code emission into many files allows for more easy understanding of what's actually going on.

-- Many of the functions here make calls to functions in 'CodeEmitterUtils' to acheive their goal. Most of the functions
-- are pretty straightforward in what they do and I've tried to name variables and functions in an intelligible manner.

import Data.Generics 					(Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran
import LanguageFortranTools
import Data.Maybe
import qualified Data.Map as DMap 

import CodeEmitterUtils

generateLoop :: VarName Anno -> Expr Anno -> Expr Anno -> Fortran Anno -> Fortran Anno
generateLoop r_iter start end fortran = For nullAnno nullSrcSpan r_iter start end step fortran
					where
						step = Con nullAnno nullSrcSpan "1"

generateWorkGroupReduction :: [VarName Anno] -> VarName Anno -> Fortran Anno -> Fortran Anno
generateWorkGroupReduction reductionVars redIter codeSeg  = if assignments == [] then error ("generateWorkGroupReduction: assignments == []\nrv: " ++ (show reductionVars) ++ "\ncodeseg: " ++ (show codeSeg)) else resultantCode
					where
						assignments = everything (++) (mkQ [] (generateWorkGroupReduction_assgs reductionVars redIter)) codeSeg
						resultantCode = foldl1 (\accum item -> appendFortran_recursive item accum) assignments

generateWorkGroupReduction_assgs :: [VarName Anno] -> VarName Anno -> Fortran Anno -> [Fortran Anno]
generateWorkGroupReduction_assgs reductionVars redIter (Assg _ _ expr1 expr2) 	| isReductionExpr = resultantAssg
																				| otherwise = []
					where 
						isReductionExpr = usesVarName_list reductionVars expr1
						resultantAssg = case extractPrimaryReductionOp expr1 expr2 of
											Just op -> [Assg nullAnno nullSrcSpan localReductionVar (Bin nullAnno nullSrcSpan op localReductionVar localReductionArray)]
											Nothing -> case extractPrimaryReductionFunction expr1 expr2 of
														"" -> []
														funcName -> [Assg nullAnno nullSrcSpan localReductionVar (Var nullAnno nullSrcSpan [(VarName nullAnno funcName, [localReductionVar, localReductionArray])])]
						localReductionArray = generateArrayVar (generateLocalReductionArray (head (extractVarNames expr1))) [(generateVar redIter)]
						localReductionVar = generateVar (generateLocalReductionVar (head (extractVarNames expr1)))
generateWorkGroupReduction_assgs reductionVars redIter codeSeg = []

generateFinalHostReduction :: [VarName Anno] -> VarName Anno -> Fortran Anno -> Fortran Anno
generateFinalHostReduction reductionVars redIter codeSeg  = resultantCode
					where
						assignments = everything (++) (mkQ [] (generateFinalHostReduction_assgs reductionVars redIter)) codeSeg
						resultantCode = foldl1 (\accum item -> appendFortran_recursive item accum) assignments

generateFinalHostReduction_assgs :: [VarName Anno] -> VarName Anno -> Fortran Anno -> [Fortran Anno]
generateFinalHostReduction_assgs reductionVars redIter (Assg _ _ expr1 expr2) 	| isReductionExpr = resultantAssg
																				| otherwise = []
					where 
						isReductionExpr = usesVarName_list reductionVars expr1
						resultantAssg = case extractPrimaryReductionOp expr1 expr2 of
											Just op -> [Assg nullAnno nullSrcSpan finalReductionVar (Bin nullAnno nullSrcSpan op finalReductionVar finalReductionArray)]
											Nothing -> case extractPrimaryReductionFunction expr1 expr2 of
														"" -> []
														funcName -> [Assg nullAnno nullSrcSpan finalReductionVar (Var nullAnno nullSrcSpan [(VarName nullAnno funcName, [finalReductionVar, finalReductionArray])])]
						finalReductionArray = generateArrayVar (generateGlobalReductionArray (head (extractVarNames expr1))) [(generateVar redIter)]
						finalReductionVar = generateVar (head (extractVarNames expr1))
generateFinalHostReduction_assgs reductionVars redIter codeSeg = []

generateGlobalWorkItemsExpr :: [(VarName Anno, Expr Anno, Expr Anno, Expr Anno)] -> Expr Anno
generateGlobalWorkItemsExpr loopVars = generateProductExpr_list (map (generateLoopIterationsExpr) loopVars)

generateRelVar :: VarName Anno -> Expr Anno
generateRelVar (VarName anno str) = generateVar (VarName anno (str ++ "_rel"))

generateRangeVar :: VarName Anno -> Expr Anno
generateRangeVar (VarName anno str) = generateVar (VarName anno (str ++ "_range"))

generateLoopStartAddition :: VarName Anno -> Expr Anno -> Fortran Anno
generateLoopStartAddition varname start = generateAssgCode (generateVar varname) (generateAdditionExpr (generateRelVar varname) start)

generateRangeExpr :: VarName Anno -> Expr Anno -> Expr Anno -> Fortran Anno
generateRangeExpr varname start end = generateAssgCode (generateRangeVar varname) (generateSubtractionExpr end start)

generateLoopInitialisers :: [(VarName Anno, Expr Anno, Expr Anno, Expr Anno)] -> Expr Anno -> Maybe(Expr Anno) -> [Fortran Anno]
generateLoopInitialisers ((var, start, end, step):[]) iterator Nothing 
			= 	[Assg nullAnno nullSrcSpan 
				(generateRelVar var)
				iterator,
				generateLoopStartAddition var start] 
generateLoopInitialisers ((var, start, end, step):[]) iterator (Just offset) 
			= 	[generateRangeExpr var start end,
				Assg nullAnno nullSrcSpan 
				(generateRelVar var)
				(offset),
				generateLoopStartAddition var start]

generateLoopInitialisers ((var, start, end, step):xs) iterator Nothing 
			= 	[generateRangeExpr var start end,
				Assg nullAnno nullSrcSpan 
				(generateRelVar var)
				(Bin nullAnno nullSrcSpan (Div nullAnno)  iterator multipliedExprs),
				generateLoopStartAddition var start]
				++
				generateLoopInitialisers xs iterator (Just nextOffset)
					where
						nextOffset = generateSubtractionExpr_list ([iterator] ++ [generateProductExpr_list ([generateRelVar var] ++ followingRangeExprs)])
						followingRangeExprs = map (\(v,_,_,_) -> generateRangeVar v) xs
						multipliedExprs = generateProductExpr_list followingRangeExprs 
generateLoopInitialisers ((var, start, end, step):xs) iterator (Just offset) 
			= 	[generateRangeExpr var start end,
				Assg nullAnno nullSrcSpan (generateRelVar var)
					(Bin nullAnno nullSrcSpan (Div nullAnno) 
						offset
						multipliedExprs),
				generateLoopStartAddition var start]
				++
				generateLoopInitialisers xs iterator (Just nextOffset)
					where
						nextOffset = generateSubtractionExpr_list ([offset] ++ [generateProductExpr_list ([generateRelVar var] ++ followingRangeExprs)])
						followingRangeExprs = map (\(v,_,_,_) -> generateRangeVar v) xs
						multipliedExprs = generateProductExpr_list followingRangeExprs 

generateProductExpr_list :: [Expr Anno] -> Expr Anno
generateProductExpr_list (x:[]) = x
generateProductExpr_list (x:xs) = Bin nullAnno nullSrcSpan (Mul nullAnno) x (generateProductExpr_list xs)

generateSubtractionExpr_list :: [Expr Anno] -> Expr Anno
generateSubtractionExpr_list (x:[]) = x
generateSubtractionExpr_list (x:xs) = Bin nullAnno nullSrcSpan (Minus nullAnno) x (generateSubtractionExpr_list xs)

generateLoopIterationsExpr :: (VarName Anno, Expr Anno, Expr Anno, Expr Anno) -> Expr Anno
generateLoopIterationsExpr (var, (Con _ _ "1"), end, (Con _ _ "1")) = end
generateLoopIterationsExpr (var, start, end, (Con _ _ "1")) = (Bin nullAnno nullSrcSpan (Plus nullAnno)
																(generateSubtractionExpr end start) 
																(generateIntConstant 1))
generateLoopIterationsExpr (var, (Con _ _ "1"), end, step) = Bin nullAnno nullSrcSpan (Div nullAnno) 
																end
																step
generateLoopIterationsExpr (var, start, end, step) = Bin nullAnno nullSrcSpan (Div nullAnno) 
														(Bin nullAnno nullSrcSpan (Plus nullAnno)
															(generateSubtractionExpr_list [end, start]) 
															(generateIntConstant 1))
														step

generateKernelDeclarations :: Program Anno -> Fortran Anno -> ([Decl Anno], [Decl Anno], [Decl Anno])
generateKernelDeclarations prog (OpenCLMap _ _ r w _ _) = (readDecls, writtenDecls, generalDecls)
				where
					readArgs = listSubtract r w
					writtenArgs = listSubtract w r
					generalArgs = listIntersection w r
					
					readDecls = map (\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (In nullAnno) prog)) readArgs
					writtenDecls = map (\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (Out nullAnno) prog)) writtenArgs
					generalDecls = map (\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (InOut nullAnno) prog)) generalArgs
generateKernelDeclarations prog (OpenCLReduce _ _ r w _ rv _) = (readDecls, writtenDecls, generalDecls_withReductions)
				where
					reductionVarNames = map (fst) rv

					readArgs = listSubtract (listSubtract r w) reductionVarNames
					writtenArgs = listSubtract (listSubtract w r) reductionVarNames
					generalArgs = listSubtract (listIntersection w r) reductionVarNames

					readDecls = map (\x -> fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (In nullAnno) prog)) readArgs
					writtenDecls = map (\x -> fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (Out nullAnno) prog)) writtenArgs
					generalDecls = map (\x -> fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (InOut nullAnno) prog)) generalArgs

					globalReductionDecls = (map (\x -> declareGlobalReductionArray x (nunitsVar) (prog)) reductionVarNames)

					generalDecls_withReductions = generalDecls ++ globalReductionDecls
generateKernelDeclarations prog (OpenCLBufferRead _ _ varName) = (readDecls, [], [])
				where
					readDecls = [(\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (In nullAnno) prog)) varName]
generateKernelDeclarations prog (OpenCLBufferWrite _ _ varName) = ([], writtenDecls, [])
				where
					writtenDecls = [(\x ->fromMaybe (generateImplicitDecl x) (adaptOriginalDeclaration_intent x (Out nullAnno) prog)) varName]

adaptOriginalDeclaration_intent :: VarName Anno -> IntentAttr Anno -> Program Anno -> Maybe(Decl Anno)
adaptOriginalDeclaration_intent varname intent program = case decl_list of
														[] -> Nothing
														_ -> Just (decl)
			where 
				decl_list = extractDeclaration_varname varname program
				decl = case containsParameterAttr (head decl_list) of
							True -> applyGeneratedSrcSpans (head decl_list)
							False -> applyGeneratedSrcSpans (applyIntent (intent) (head decl_list))


generateSizeStatements_decls :: String -> [Decl Anno] -> ([Decl Anno], [Fortran Anno])
generateSizeStatements_decls tabs decls = (sizeDeclarations ++ scalarPointerDeclarations, shapeStatements)
		where 
			shapeStatements = foldl (\accum varname -> accum ++ [generateSizeStatement varname]) [] vars_onlyArrays
			sizeDeclarations = foldl (\accum (varname, rank) -> accum ++ [generateSizeDecl (varSizeVarName varname) rank]) [] varsWithRanks_arrays

			allVars = map (extractAssigneeFromDecl) decls 

			dimensionRanks = map (getDeclRank) (decls)
			varsWithRanks = zip allVars dimensionRanks
			varsWithRanks_arrays = map (\(var, rank) -> if rank == 0 then (scalarPointerVarName var, 1) else (var, rank)) varsWithRanks

			vars_onlyArrays = map (\(var, rank) -> if rank == 0 then scalarPointerVarName var else var) varsWithRanks
			
			scalarDecls = filter (\x -> 0 == getDeclRank x) decls
			scalarPointerDeclarations = map (\x -> removeIntentFromDecl (declareScalarPointer_decl x)) scalarDecls


generateSizeDecl :: VarName Anno -> Int -> Decl Anno
generateSizeDecl varname rank = Decl nullAnno nullSrcSpan [(generateVar varname, (NullExpr nullAnno nullSrcSpan), Nothing)] 
										(BaseType nullAnno (Integer nullAnno) [Dimension nullAnno [(NullExpr nullAnno nullSrcSpan, rankConst)]] (NullExpr nullAnno nullSrcSpan) (eight))
		where
			eight = generateIntConstant 8
			rankConst = generateIntConstant rank

generateSizeStatement :: VarName Anno -> Fortran Anno
generateSizeStatement varname = Assg nullAnno nullSrcSpan assignee assignment
		where
			assignee = generateVar (varSizeVarName varname)
			assignment = Var nullAnno nullSrcSpan [(VarName nullAnno "shape", [generateVar varname])]

generateReductionArrayAssignment tabs accessor accum ((VarName _ s1),(VarName _ s2)) = accum++tabs++s1++"("++(outputExprFormatting accessor)++") = "++s2++"\n"

generateImplicitDecl :: VarName Anno -> Decl Anno
generateImplicitDecl var = Decl nullAnno nullSrcSpan [(generateVar var, (NullExpr nullAnno nullSrcSpan), Nothing)] (BaseType nullAnno (Real nullAnno) [] (NullExpr nullAnno nullSrcSpan) (NullExpr nullAnno nullSrcSpan))

declareGlobalReductionArray :: VarName Anno -> Expr Anno -> Program Anno -> Decl Anno
declareGlobalReductionArray varname arraySize program = decl			
			where
				decl_list = extractDeclaration_varname varname program
				foundDecl = case decl_list of
								[] -> generateImplicitDecl varname
								_ -> head decl_list
				one = generateIntConstant 1
				newVarName = generateGlobalReductionArray varname
				decl = applyIntent (Out nullAnno) (replaceAllOccurences_varname (addDimension foundDecl one arraySize) varname newVarName)

declareScalarPointer :: VarName Anno -> Program Anno -> Decl Anno
declareScalarPointer varname program = decl
			where
				decl_list = extractDeclaration_varname varname program
				foundDecl = case decl_list of
								[] -> generateImplicitDecl varname
								_ -> head decl_list
				one = generateIntConstant 1
				nullExpr = NullExpr nullAnno nullSrcSpan
				newVarName = scalarPointerVarName varname
				decl = replaceAllOccurences_varname (addDimension foundDecl nullExpr one) varname newVarName

declareLocalReductionArray :: VarName Anno -> Expr Anno -> Program Anno -> Decl Anno
declareLocalReductionArray varname arraySize program = decl
			where
				decl_list = extractDeclaration_varname varname program
				foundDecl = case decl_list of
								[] -> generateImplicitDecl varname
								_ -> head decl_list
				one = generateIntConstant 1
				newVarName = generateLocalReductionArray varname
				decl = addDimension (replaceAllOccurences_varname foundDecl varname newVarName) one arraySize

adaptOriginalDeclaration_varname :: VarName Anno -> VarName Anno -> Program Anno -> Decl Anno
adaptOriginalDeclaration_varname varname newVarname program = decl
			where 
				decl_list = extractDeclaration_varname varname program
				foundDecl = case decl_list of
								[] -> generateImplicitDecl varname
								_ -> head decl_list
				decl = applyGeneratedSrcSpans (replaceAllOccurences_varname foundDecl varname newVarname)

generateArgList :: [VarName Anno] -> ArgList Anno
generateArgList [] = ArgList nullAnno (NullExpr nullAnno nullSrcSpan)
generateArgList vars = ArgList nullAnno (generateESeq vars)