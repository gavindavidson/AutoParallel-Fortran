module CodeEmitterUtils

where

-- This module was written to separate some of the functions out of CodeEmitter.hs. It houses all sorts of functions that acheive many
-- different goals during the transformations and analyses that are performed during code emission. All the functions are very simple
-- and some merely have hardcoded outputs to provide some degree of consistency during code emission

import Data.Generics 					(Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran
import LanguageFortranTools
import qualified Data.Map as DMap 

type KernelArgsIndexMap = DMap.Map (VarName Anno) Int

localMemSpaceAcc = " !$ACC MemSpace local"
globalMemSpaceAcc = "!$ACC MemSpace global"
localChunkSize = generateVar (VarName nullAnno "local_chunk_size")
startPosition  = generateVar (VarName nullAnno "start_position")
chunk_size = generateVar chunk_size_varname
chunk_size_varname = VarName nullAnno "chunk_size"
localMemBarrier = "call barrier(CLK_LOCAL_MEM_FENCE)\n"
nthVar = generateVar (VarName nullAnno "NTH")
nunitsVar = generateVar (VarName nullAnno "NUNITS")
numGroupsVarName = VarName nullAnno "num_groups"
numGroupsVar = generateVar numGroupsVarName
stateVarName = VarName nullAnno "state"
statePtrVarName = VarName nullAnno "state_ptr"
statePtrDecl = Decl nullAnno nullSrcSpan [(statePtrVar, NullExpr nullAnno nullSrcSpan, Nothing)] 
									(BaseType nullAnno (Integer nullAnno) [Dimension nullAnno [(NullExpr nullAnno nullSrcSpan, generateIntConstant 1)]] (NullExpr nullAnno nullSrcSpan) (NullExpr nullAnno nullSrcSpan))
stateVar = generateVar stateVarName
stateVarDecl = Decl nullAnno nullSrcSpan [(stateVar, NullExpr nullAnno nullSrcSpan, Nothing)] 
									(BaseType nullAnno (Integer nullAnno) [] (NullExpr nullAnno nullSrcSpan) (NullExpr nullAnno nullSrcSpan))
statePtrVar = generateVar statePtrVarName
initModuleName moduleName = moduleName ++ "_init"
hostModuleName moduleName = moduleName ++ "_host"

--	Function takes a list of lines from the original source and an object representing a range of line numbers and reproduces the original code
--	in the range of those line numbers.
extractOriginalCode :: String -> [String] -> SrcSpan -> String
extractOriginalCode tabs originalLines src = orignalFileChunk
					where 
						((SrcLoc f lineStart columnStart), (SrcLoc _ lineEnd columnEnd)) = src
						orignalFileChunk = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [lineStart..lineEnd]


scalarPointerVar :: VarName Anno -> Expr Anno
scalarPointerVar varname = generateVar (scalarPointerVarName varname)

scalarPointerVarName :: VarName Anno -> VarName Anno
scalarPointerVarName (VarName _ str) = VarName nullAnno (str ++ "_ptr")

varSizeVar :: VarName Anno -> Expr Anno
varSizeVar varName = generateVar (varSizeVarName varName)

varSizeVarName :: VarName Anno -> VarName Anno
varSizeVarName (VarName _ str) = VarName nullAnno (str ++ "_sz")

varBufVar :: VarName Anno -> Expr Anno
varBufVar varName = generateVar (varSizeVarName varName)

varBufVarName :: VarName Anno -> VarName Anno
varBufVarName (VarName _ str) = VarName nullAnno (str ++ "_buf")

--	Function is used (along with "getFirstBlockSrc") by "produceCode_progUnit" to determine which lines of the original
--	source can be taken as is. It is used to determine where the first Fortran nodes of the AST appear in the source
--	because the Fortran nodes are the ones that have been transformed.
getFirstFortranSrc :: Block Anno -> [SrcSpan]
getFirstFortranSrc (Block _ _ _ _ _ fortran) = [srcSpan fortran]

getFirstBlockSrc :: Block Anno -> [SrcSpan]
getFirstBlockSrc codeSeg = [srcSpan codeSeg]

getDimensionExprs :: Attr Anno -> [(Expr Anno, Expr Anno)]
getDimensionExprs (Dimension _ exprs) = exprs
getDimensionExprs _ = []

insertDecls :: [Decl Anno] -> Decl Anno -> Decl Anno
insertDecls newDecls declTree = insertDecl newDeclsTree declTree
		where
			newDeclsTree = constructDeclTree newDecls

constructDeclTree :: [Decl Anno] -> Decl Anno
constructDeclTree [] = NullDecl nullAnno nullSrcSpan
constructDeclTree (decl:[]) = decl
constructDeclTree (decl:decls) = DSeq nullAnno decl (constructDeclTree decls)

--	Given a decl to insert and the start of the decl tree, insert the new decl and return the new decl tree
insertDecl :: Decl Anno -> Decl Anno -> Decl Anno 
insertDecl newDecl (DSeq anno decl1 decl2) = DSeq anno decl1 (insertDecl newDecl decl2)
insertDecl newDecl declLeaf = DSeq nullAnno declLeaf newDecl

convertScalarToOneDimArray :: Decl Anno -> Decl Anno
convertScalarToOneDimArray decl 	|	isScalar = addDimension decl zero one
									|	otherwise = decl
		where
			isScalar = (getDeclRank decl == 0)
			one = generateIntConstant 1
			zero = generateIntConstant 0


adaptForReadScalarDecls :: [VarName Anno] -> ([Decl Anno], [Decl Anno], [Decl Anno]) -> ([Decl Anno], [Decl Anno], [Decl Anno], Fortran Anno, [VarName Anno])
adaptForReadScalarDecls allArgs (readDecls, writtenDecls, generalDecls)  = (finalReadDecls, writtenDecls, finalGeneralDecls, ptrAssignments_fseq, allArgs_ptrAdaption)
		where
			readScalars = map (removeIntentFromDecl) (filter (\x -> (getDeclRank x) == 0) readDecls)
			generalScalars = map (removeIntentFromDecl) (filter (\x -> (getDeclRank x) == 0) generalDecls)
			scalars = map (extractAssigneeFromDecl) (readScalars ++ generalScalars)

			readDecls_noScalars = filter (\x -> (getDeclRank x) /= 0) readDecls -- listSubtract readDecls readScalars
			generalDecls_noScalars = filter (\x -> (getDeclRank x) /= 0) generalDecls --listSubtract generalDecls generalScalars

			readPtrs = map (declareScalarPointer_decl) readScalars
			generalPtrs = map (declareScalarPointer_decl) generalScalars

			ptrAssignments_list = map (\decl -> generatePtrScalarAssignment (extractAssigneeFromDecl decl)) (readScalars ++ generalScalars)
			ptrAssignments_fseq = generateFSeq ptrAssignments_list

			finalReadDecls = readDecls_noScalars ++ 
								readScalars ++ readPtrs
			finalGeneralDecls = generalDecls_noScalars ++ generalScalars ++ generalPtrs

			allArgs_ptrAdaption = map (\var -> if elem var scalars then scalarPointerVarName var else var) allArgs

addDimension :: Decl Anno -> Expr Anno -> Expr Anno -> Decl Anno
addDimension decl start end = newDecl
			where 
				dimensions = everything (++) (mkQ [] extractDimensionAttr) decl
				newDecl = case dimensions of
							[] -> everywhere (mkT (addNewDimensionClaus start end)) decl
							_ -> everywhere (mkT (appendDimension start end)) decl

getDeclRank :: Decl Anno -> Int
getDeclRank decl 	|	extractedDimensions == [] = 0
					|	otherwise = dimensionRank
		where
			extractedDimensions = everything (++) (mkQ [] extractDimensionAttr) decl
			dimensionRank = length (getDimensionExprs (head extractedDimensions))

removeIntentFromDecl :: Decl Anno -> Decl Anno
removeIntentFromDecl decl = gmapT (mkT removeIntentFromType) decl


declareScalarPointer_decl :: Decl Anno -> Decl Anno
declareScalarPointer_decl decl = resultDecl
			where
				one = generateIntConstant 1
				zero = generateIntConstant 0
				varname = extractAssigneeFromDecl decl
				newVarName = scalarPointerVarName varname
				resultDecl = replaceAllOccurences_varname (addDimension decl zero one) varname newVarName

extractDimensionAttr :: Attr Anno -> [Attr Anno]
extractDimensionAttr attr = case attr of
								Dimension _ _ -> [attr]
								_ -> [] 


addNewDimensionClaus :: Expr Anno -> Expr Anno -> [Attr Anno] -> [Attr Anno]
addNewDimensionClaus start end [] = [(Dimension nullAnno [(start, end)])]
addNewDimensionClaus start end (attr:attrList) = case attr of
										Intent _ _ -> [attr] ++ attrList
										_ -> [attr] ++ addNewDimensionClaus start end attrList

generatePtrScalarAssignment :: VarName Anno -> Fortran Anno
generatePtrScalarAssignment var = Assg nullAnno nullSrcSpan assignee assignment
		where
			ptrName = scalarPointerVarName var
			assignee = generateVar var
			assignment = generateArrayVar ptrName [generateIntConstant 1]

appendDimension :: Expr Anno -> Expr Anno -> Attr Anno -> Attr Anno
appendDimension start end (Dimension anno lst) = Dimension anno (lst ++ [(start, end)])
appendDimension start end att = att

removeIntentFromType :: Type Anno -> Type Anno
removeIntentFromType (BaseType anno btype attrList expr1 expr2) = (BaseType anno btype newAttrList expr1 expr2)
		where
			newAttrList = filter (\x -> not (isIntent x)) attrList
removeIntentFromType (ArrayT  anno exprList btype attrList expr1 expr2) = (ArrayT  anno exprList btype newAttrList expr1 expr2)
		where
			newAttrList = filter (\x -> not (isIntent x)) attrList

isIntent :: Attr Anno -> Bool
isIntent (Intent _ _) = True
isIntent _ = False

extractDeclaration_varname :: VarName Anno -> Program Anno -> [Decl Anno]
extractDeclaration_varname varname program = everything (++) (mkQ [] (extractDeclaration_varname' varname)) program

extractDeclaration_varname' :: VarName Anno -> Decl Anno -> [Decl Anno]
extractDeclaration_varname' varname  (Decl anno src lst typ)  	| firstHasVar || secondHasVar = [Decl anno src lst typ]
														| otherwise = []
			where
				firstExprs = map (\(expr, _, _) -> expr) lst
				secondExprs = map (\(_, expr, _) -> expr) lst

				firstHasVar = foldl (\accum item -> accum || usesVarName_list [varname] item) False firstExprs
				secondHasVar = foldl (\accum item -> accum || usesVarName_list [varname] item) False secondExprs
extractDeclaration_varname' varname decl = []

getOriginalDeclaration :: [String] -> VarName Anno -> Program Anno -> Maybe(String)
getOriginalDeclaration originalLines varname program = case declSrc_list of
														[] -> Nothing
														_ -> Just (extractOriginalCode "" originalLines declSrc)
			where 
				declSrc_list = everything (++) (mkQ [] (extractDeclaration_varnameSrcSpan varname)) program
				declSrc = head declSrc_list


extractDeclaration_varnameSrcSpan :: VarName Anno -> Decl Anno -> [SrcSpan]
extractDeclaration_varnameSrcSpan varname (Decl _ src lst _) 	| firstHasVar || secondHasVar = [src]
														| otherwise = []
			where
				firstExprs = map (\(expr, _, _) -> expr) lst
				secondExprs = map (\(_, expr, _) -> expr) lst

				firstHasVar = foldl (\accum item -> accum || usesVarName_list [varname] item) False firstExprs
				secondHasVar = foldl (\accum item -> accum || usesVarName_list [varname] item) False secondExprs
extractDeclaration_varnameSrcSpan varname decl = []

containsParameterAttr :: Decl Anno -> Bool
containsParameterAttr decl = foldl (||) False (gmapQ (mkQ False paramCheck_type) decl)

paramCheck_type :: Type Anno -> Bool
paramCheck_type (BaseType _ baseT attrList _ _) = foldl (\accum item -> accum || paramCheck_attr item) False attrList
paramCheck_type (ArrayT _ _ baseT attrList _ _) = foldl (\accum item -> accum || paramCheck_attr item) False attrList

paramCheck_attr :: Attr Anno -> Bool
paramCheck_attr (Parameter _) = True 
paramCheck_attr _ = False

extractintentAttrs :: IntentAttr Anno -> [IntentAttr Anno]
extractintentAttrs intentAttr = [intentAttr]

replaceIntent :: IntentAttr Anno -> IntentAttr Anno -> IntentAttr Anno
replaceIntent newIntent oldIntent = newIntent

addIntent :: IntentAttr Anno -> [Attr Anno] -> [Attr Anno]
addIntent intent [] = [Intent nullAnno intent]
addIntent intent (attr:attrList) = case attr of
										Intent _ _ -> [attr] ++ attrList
										_ -> [attr] ++ addIntent intent attrList

anyChildGenerated :: Fortran Anno -> Bool
anyChildGenerated ast = everything (||) (mkQ False isGenerated) ast

isGenerated :: Fortran Anno -> Bool
isGenerated codeSeg = f /= "<unknown>" || (lineStart == -1 && lineEnd == -1) || f == "generated"
			where
				((SrcLoc f lineStart columnStart), (SrcLoc _ lineEnd columnEnd)) = srcSpan codeSeg


getGlobalID :: Expr Anno ->  Expr Anno
getGlobalID globalIdVar = Var nullAnno nullSrcSpan [(VarName nullAnno "get_global_id", [globalIdVar, Con nullAnno nullSrcSpan "0"])]

getGroupID :: Expr Anno ->  Expr Anno
getGroupID groupIdVar = Var nullAnno nullSrcSpan [(VarName nullAnno "get_group_id", [groupIdVar, Con nullAnno nullSrcSpan "0"])]

getNumberGroups :: Expr Anno ->  Expr Anno
getNumberGroups numberGroupsVar = Var nullAnno nullSrcSpan [(VarName nullAnno "get_num_groups", [numberGroupsVar, Con nullAnno nullSrcSpan "0"])]

getGroupSize :: Expr Anno ->  Expr Anno
getGroupSize groupSizeVar = Var nullAnno nullSrcSpan [(VarName nullAnno "get_group_size", [groupSizeVar, Con nullAnno nullSrcSpan "0"])]

getLocalSize :: Expr Anno -> Expr Anno
getLocalSize localSizeVar = Var nullAnno nullSrcSpan [(VarName nullAnno "get_local_size", [localSizeVar, Con nullAnno nullSrcSpan "0"])]

getLocalId :: Expr Anno -> Expr Anno
getLocalId localIdVar = Var nullAnno nullSrcSpan [(VarName nullAnno "get_local_id", [localIdVar, Con nullAnno nullSrcSpan "0"])]

-- Formally, this function produced a varname that had not been used in a supplied list of varnames, to ensure that the iterator
-- variable didn't clash. This feature has been scrapped for the time being as it causes complications. 
generateReductionIterator :: VarName Anno
generateReductionIterator = VarName nullAnno "r_iter"

reductionIteratorDecl :: String
reductionIteratorDecl = "Integer :: " ++ (varNameStr generateReductionIterator)

collectDecls :: [Decl Anno] -> Decl Anno -> [Decl Anno]
collectDecls previousDecls currentDecl = mergeDeclWithPrevious_recurse previousDecls currentDecl

mergeDeclWithPrevious_recurse :: [Decl Anno] -> Decl Anno -> [Decl Anno]
mergeDeclWithPrevious_recurse (listDecl:decls) currentDecl 	|	matchingVarNames && currentDeclIntentAttrs == listDeclIntentAttrs = currentDecl:decls
															|	matchingVarNames && currentDeclIntentAttrs /= listDeclIntentAttrs = adaptedIntentDecl:decls
															|	otherwise = listDecl:(mergeDeclWithPrevious_recurse decls currentDecl)
			where
				listDeclName = extractAssigneeFromDecl listDecl
				currentDeclName = extractAssigneeFromDecl currentDecl
				matchingVarNames = listDeclName == currentDeclName

				listDeclIntentAttrs = everything (++) (mkQ [] extractintentAttrs) listDecl
				currentDeclIntentAttrs = everything (++) (mkQ [] extractintentAttrs) currentDecl

				adaptedIntentDecl = applyIntent (InOut nullAnno) listDecl
mergeDeclWithPrevious_recurse [] currentDecl = [currentDecl]


extractKernelArguments :: Fortran Anno -> [VarName Anno]
extractKernelArguments (OpenCLMap _ _ r w _ _) = listRemoveDuplications (r ++ w)
extractKernelArguments (OpenCLReduce _ _ r w _ rv _) = listRemoveDuplications ((listSubtract (r ++ w) rvVarNames) ++ (map (\x -> generateGlobalReductionArray (fst x)) rv))
				where
					rvVarNames = map (fst) rv
extractKernelArguments _ = []

generateLocalReductionArray (VarName anno str) = VarName anno ("local_" ++ str ++ "_array")
generateGlobalReductionArray (VarName anno str) = VarName anno ("global_" ++ str ++ "_array")
generateLocalReductionArrayArgStr accum item = accum ++ "\n" ++ tabInc ++ "__local " ++ varNameStr item
generateGloablReductionArrayArgStr accum item = accum ++ "\n" ++ tabInc ++ "__global " ++ varNameStr item
generateLocalReductionVar (VarName anno str) = VarName anno ("local_" ++ str)

applyIntent :: IntentAttr Anno -> Decl Anno -> Decl Anno
applyIntent intent decl =  newDecl
			where 
				intentAttrs = everything (++) (mkQ [] extractintentAttrs) decl
				newDecl = case intentAttrs of
							[] -> everywhere (mkT (addIntent intent)) decl
							_ -> everywhere (mkT (replaceIntent intent)) decl

--	The following functions are used to define names for output files from the input files' names.
getModuleName :: String -> String
getModuleName filename = head (splitOnChar '.' (last (splitOnChar '/' filename)))

splitOnChar :: Char -> String -> [String]
splitOnChar char str = splitOnChar' char "" str

splitOnChar' :: Char -> String -> String -> [String]
splitOnChar' char current (x:xs) 	|	char == x = current:(splitOnChar' char "" xs)
									|	otherwise = splitOnChar' char (current ++ [x]) xs
splitOnChar' _ current []			=	[current]

stripDeclAttrs :: Decl Anno -> Decl Anno
stripDeclAttrs decl = everywhere (mkT stripAttrs) decl

stripAttrs :: [Attr Anno] -> [Attr Anno]
stripAttrs a = []