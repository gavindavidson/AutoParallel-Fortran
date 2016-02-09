module CodeEmitter where

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List
import Data.List.Split
import System.IO
import System.Process

import LanguageFortranTools

emit :: String -> String -> Program [String] -> IO ()
emit filename "" ast = do
				let newFilename = defaultFilename (splitOn "/" filename)
				originalLines <- readOriginalFileLines filename
				let code = produceCodeProg originalLines ast
				--putStr $ show (originalLines!!21)
				writeFile newFilename (show ast)
				--writeFile newFilename code
emit filename specified ast = do		
				originalLines <- readOriginalFileLines filename		
				let code = produceCodeProg originalLines ast
				--writeFile specified (show ast)
				writeFile specified code

defaultFilename :: [String] -> String
defaultFilename (x:[]) = "par_" ++ x
defaultFilename (x:xs) = x ++ "/" ++ defaultFilename xs

readOriginalFileLines :: String -> IO ([String])
readOriginalFileLines filename = do
				content <- readFile filename
				let contentLines = lines content
				return contentLines

produceCodeProg :: [String] -> Program [String] -> String
produceCodeProg originalLines prog = everything (++) (mkQ "" (produceCodeBlock originalLines)) prog

produceCodeBlock :: [String] -> Block [String] -> String
produceCodeBlock originalLines block = foldl (++) "" (gmapQ (mkQ "" (produceCode originalLines)) block)

produceCode :: [String] -> Fortran [String] -> String
produceCode originalLines codeSeg 	|	anyChildGenerated codeSeg || isGenerated codeSeg = foldl (++) "" (gmapQ (mkQ "" (produceCode originalLines)) codeSeg)
									|	otherwise = orignalFileChunk ++ "\n"
										where 
											((SrcLoc f lineStart columnStart), (SrcLoc _ lineEnd columnEnd)) = srcSpan codeSeg
											orignalFileChunk = foldl (\accum item -> accum ++ "\n" ++ (originalLines!!(item-1))) "" [lineStart..lineEnd]

anyChildGenerated :: Fortran [String] -> Bool
anyChildGenerated ast = everything (||) (mkQ False isGenerated) ast

isGenerated :: Fortran [String] -> Bool
isGenerated codeSeg = f == "generated"
			where
				((SrcLoc f lineStart columnStart), (SrcLoc _ lineEnd columnEnd)) = srcSpan codeSeg

--	Returns an AST representing a set of assignments that determine the values of the loop variables that are being parallelised for a
--	given global_id value.
flattenLoopConditions :: Maybe (VarName p) -> (VarName p) -> [(VarName p, Expr p, Expr p, Expr p)] -> Fortran p
flattenLoopConditions prev globalId ((var, start, end, step):[]) = Assg 
																		(tag globalId) 
																		nullSrcSpan 
																		(Var (tag globalId) nullSrcSpan [(var, [])])
																		(primitiveMod globalId end)
flattenLoopConditions prev globalId ((var, start, end, step):xs) = 	FSeq 
																	(tag globalId) 
																	nullSrcSpan (
																		Assg 
																		(tag globalId) 
																		nullSrcSpan (
																			Var (tag globalId) nullSrcSpan [(var, [])])
																		(flattenCondition_div globalId prev (multiplyLoopConditions xs) -- DIVISOR
																			)
																		)
																	 (flattenLoopConditions (Just var) globalId xs) -- FSeq p SrcSpan (Fortran p) (Fortran p) 

--	Function returns an AST represnting a standard division that is performed to calculate loop variable values.
flattenCondition_div :: VarName p -> Maybe (VarName p) -> Expr p -> Expr p
flattenCondition_div globalId (Just prev) divisor = Bin 
														(tag globalId) 
														nullSrcSpan 
														(Div (tag globalId)) (
															Bin 
																(tag globalId) 
																nullSrcSpan 
																(Minus (tag globalId)) 
																(Var 
																	(tag globalId) 
																	nullSrcSpan 
																	[(globalId, [])]) 
																(Var 
																	(tag globalId) 
																	nullSrcSpan 
																	[(prev, [])]))
														divisor
flattenCondition_div globalId Nothing divisor = 	Bin 
														(tag globalId) 
														nullSrcSpan 
														(Div (tag globalId)) 
														(Var 
															(tag globalId) 
															nullSrcSpan 
															[(globalId, [])]) 
														divisor

--	Function returns an AST represnting a standard modulus calculation that is performed to calculate loop variable values.
flattenCondition_mod :: VarName p -> Maybe (VarName p) -> Expr p -> Expr p
flattenCondition_mod globalId (Just prev) divisor = Bin 
														(tag globalId) 
														nullSrcSpan 
														(Div (tag globalId)) 
														(Var 
															(tag globalId) 
															nullSrcSpan 
															[(globalId, [])]) 
														divisor 

--	Fortran does not have a modulus operator as standard. Therefore, this function returns an AST represnting a modulus calculation
--	that only uses primitive operators (+, -, *, /)
primitiveMod :: VarName p -> Expr p -> Expr p 
primitiveMod quotient divisor = Bin 
									(tag quotient) 
									nullSrcSpan 
									(Minus (tag quotient)) 
									(Var 
										(tag quotient) 
										nullSrcSpan 
										[(quotient, [])]) 
									(Bin 
										(tag quotient) 
										nullSrcSpan 
										(Mul (tag quotient)) 
										(Bin 
											(tag quotient) 
											nullSrcSpan 
											(Div (tag quotient)) 
											(Var 
												(tag quotient) 
												nullSrcSpan 
												[(quotient, [])]) 
											divisor) 
										divisor)

-- 	Used by flattenLoopConditions to produce an expression that multiplies together the loop variable dimensions. 
--	This will likely be changed.
multiplyLoopConditions :: [(VarName p, Expr p, Expr p, Expr p)] -> Expr p
multiplyLoopConditions ((var, start, end, step):[]) = end
multiplyLoopConditions ((var, start, end, step):xs) = Bin (tag var) nullSrcSpan (Mul (tag var)) end (multiplyLoopConditions xs)