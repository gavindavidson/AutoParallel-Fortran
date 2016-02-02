module CodeEmitter where

-- import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran
import Data.Char

import LanguageFortranTools

--	This file will contain the code that actually produces the parallel version of the input program code. Currently it only
--	houses old code that is likely going to be heavily modified before use. 

--	Returns an AST representing a set of assignments that determine the values of the loop variables that are being parallelised for a
--	given global_id value.
flattenLoopConditions :: Maybe (VarName p) -> (VarName p) -> [(VarName p, Expr p, Expr p, Expr p)] -> Fortran p
flattenLoopConditions prev globalId ((var, start, end, step):[]) = Assg 
																		(tag globalId) 
																		generatedSrcSpan 
																		(Var (tag globalId) generatedSrcSpan [(var, [])])
																		(primitiveMod globalId end)
flattenLoopConditions prev globalId ((var, start, end, step):xs) = 	FSeq 
																	(tag globalId) 
																	generatedSrcSpan (
																		Assg 
																		(tag globalId) 
																		generatedSrcSpan (
																			Var (tag globalId) generatedSrcSpan [(var, [])])
																		(flattenCondition_div globalId prev (multiplyLoopConditions xs) -- DIVISOR
																			)
																		)
																	 (flattenLoopConditions (Just var) globalId xs) -- FSeq p SrcSpan (Fortran p) (Fortran p) 

--	Function returns an AST represnting a standard division that is performed to calculate loop variable values.
flattenCondition_div :: VarName p -> Maybe (VarName p) -> Expr p -> Expr p
flattenCondition_div globalId (Just prev) divisor = Bin 
														(tag globalId) 
														generatedSrcSpan 
														(Div (tag globalId)) (
															Bin 
																(tag globalId) 
																generatedSrcSpan 
																(Minus (tag globalId)) 
																(Var 
																	(tag globalId) 
																	generatedSrcSpan 
																	[(globalId, [])]) 
																(Var 
																	(tag globalId) 
																	generatedSrcSpan 
																	[(prev, [])]))
														divisor
flattenCondition_div globalId Nothing divisor = 	Bin 
														(tag globalId) 
														generatedSrcSpan 
														(Div (tag globalId)) 
														(Var 
															(tag globalId) 
															generatedSrcSpan 
															[(globalId, [])]) 
														divisor

--	Function returns an AST represnting a standard modulus calculation that is performed to calculate loop variable values.
flattenCondition_mod :: VarName p -> Maybe (VarName p) -> Expr p -> Expr p
flattenCondition_mod globalId (Just prev) divisor = Bin 
														(tag globalId) 
														generatedSrcSpan 
														(Div (tag globalId)) 
														(Var 
															(tag globalId) 
															generatedSrcSpan 
															[(globalId, [])]) 
														divisor 

--	Fortran does not have a modulus operator as standard. Therefore, this function returns an AST represnting a modulus calculation
--	that only uses primitive operators (+, -, *, /)
primitiveMod :: VarName p -> Expr p -> Expr p 
primitiveMod quotient divisor = Bin 
									(tag quotient) 
									generatedSrcSpan 
									(Minus (tag quotient)) 
									(Var 
										(tag quotient) 
										generatedSrcSpan 
										[(quotient, [])]) 
									(Bin 
										(tag quotient) 
										generatedSrcSpan 
										(Mul (tag quotient)) 
										(Bin 
											(tag quotient) 
											generatedSrcSpan 
											(Div (tag quotient)) 
											(Var 
												(tag quotient) 
												generatedSrcSpan 
												[(quotient, [])]) 
											divisor) 
										divisor)

-- 	Used by flattenLoopConditions to produce an expression that multiplies together the loop variable dimensions. 
--	This will likely be changed.
multiplyLoopConditions :: [(VarName p, Expr p, Expr p, Expr p)] -> Expr p
multiplyLoopConditions ((var, start, end, step):[]) = end
multiplyLoopConditions ((var, start, end, step):xs) = Bin (tag var) generatedSrcSpan (Mul (tag var)) end (multiplyLoopConditions xs)