module TupleTable 					(TupleTable (..), getMostTuple, getLeastTuple, insertIntoTupleTable, lookupTupleTable, collapseIterTable, 
									tupleTableElementGreaterThan, tupleTableNotEmpty)

where 

import Data.Maybe
import qualified Data.Map as DMap

import LanguageFortranTools 		(listConcatUnique)

--	The TupleTable data structure is used to represent a tree like structure of tuples. it is a trie (https://en.wikipedia.org/wiki/Trie) that has all
--	its leaf nodes at the same depth. It is used by the compiler to represent combinations of loop iterator values without incuring an explosion of
-- 	space usage. Functions in this file allow for the construction and use of the TupleTable, as well as a function that collapses all of the nodes
--	at a certain depth in a subtrie into one node while keeping the children of the collapsed nodes together. This allows for the compiler to skip
--	tuples that are not going to reveal any new information during analysis.

data TupleTable = LoopIterRecord (DMap.Map Int TupleTable) 
					| Empty
					deriving (Show, Eq)

tupleTableElementGreaterThan :: [Int] -> [Int] -> Bool
tupleTableElementGreaterThan [] [] 			=	False
tupleTableElementGreaterThan (x:[]) (y:[])	=	x > y
tupleTableElementGreaterThan [] y 			=	False
tupleTableElementGreaterThan x [] 			=	True
tupleTableElementGreaterThan (x:xs) (y:ys) 	|	x > y = True
											|	x == y = tupleTableElementGreaterThan xs ys
											|	x < y = False

getMostTuple :: TupleTable -> [Int]
getMostTuple (LoopIterRecord table) = [leastValue] ++ (getMostTuple (DMap.findWithDefault Empty leastValue table))
			where 
				allowedValues = DMap.keys table
				leastValue = foldl1 (max) allowedValues
getMostTuple Empty = []

getLeastTuple :: TupleTable -> [Int]
getLeastTuple (LoopIterRecord table) = [leastValue] ++ (getLeastTuple (DMap.findWithDefault Empty leastValue table))
			where 
				allowedValues = DMap.keys table
				leastValue = foldl1 (min) allowedValues
getLeastTuple Empty = []

insertIntoTupleTable :: [Int] -> TupleTable -> TupleTable
insertIntoTupleTable [] tupleTable = tupleTable
insertIntoTupleTable (indices) Empty = createTupleTable indices
insertIntoTupleTable (indices) (LoopIterRecord table) = case DMap.lookup (head indices) table of 
															Just subTable -> LoopIterRecord (DMap.insert (head indices) (insertIntoTupleTable (tail indices) subTable) table)
															Nothing -> LoopIterRecord (DMap.insert (head indices) (createTupleTable (tail indices)) table)

collapseIterTable :: TupleTable -> TupleTable
collapseIterTable (LoopIterRecord iterTable) = foldl (\accum (key, newTable) -> insertIfNotRepresented key newTable accum) Empty (zip allowedValues subTables)
			where
				allowedValues = DMap.keys iterTable
				subTables = map (\x -> DMap.findWithDefault Empty x iterTable) allowedValues

insertIfNotRepresented :: Int -> TupleTable -> TupleTable -> TupleTable
insertIfNotRepresented key newItem Empty = LoopIterRecord (DMap.insert key newItem DMap.empty)
insertIfNotRepresented key newItem (LoopIterRecord table) = if not (elem newItem representedItems) then LoopIterRecord (DMap.insert key newItem table) else LoopIterRecord table
			where
				representedItems = map (\x -> DMap.findWithDefault Empty x table) (DMap.keys table)

createTupleTable :: [Int] -> TupleTable
createTupleTable [] = Empty
createTupleTable indices = LoopIterRecord (DMap.insert (head indices) (createTupleTable (tail indices)) DMap.empty)

lookupTupleTable :: [Int] -> TupleTable -> Maybe(TupleTable)
lookupTupleTable [index] (LoopIterRecord table) = DMap.lookup index table
lookupTupleTable (index:indices) (LoopIterRecord table) = lookupTupleTable indices (DMap.findWithDefault Empty index table)
lookupTupleTable _ _ = Nothing

tupleTableNotEmpty :: TupleTable -> Bool
tupleTableNotEmpty Empty = False
tupleTableNotEmpty _ = True