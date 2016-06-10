module TupleTable where 

import Data.Maybe
import qualified Data.Map.Strict as DMap

import LanguageFortranTools

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

joinTupleTable :: TupleTable -> TupleTable -> TupleTable
joinTupleTable Empty Empty = Empty
joinTupleTable table1 table2 = foldl (joinTupleTable_foldl table1 table2) newTable allowedValues
			where
				allowedValues = case table1 of
									LoopIterRecord a -> case table2 of 
															Empty -> DMap.keys a
															LoopIterRecord b -> listConcatUnique (DMap.keys a) (DMap.keys b)
									Empty -> 			case table2 of 
															Empty -> []
															LoopIterRecord b -> DMap.keys b
				newTable = LoopIterRecord DMap.empty

joinTupleTable_foldl :: TupleTable -> TupleTable -> TupleTable -> Int -> TupleTable
joinTupleTable_foldl (LoopIterRecord table1) (LoopIterRecord table2) (LoopIterRecord newTable) value = LoopIterRecord (DMap.insert value joinedTable newTable)
			where
				subTable1 = DMap.findWithDefault Empty value table1
				subTable2 = DMap.findWithDefault Empty value table2
				joinedTable = joinTupleTable subTable1 subTable2
joinTupleTable_foldl (LoopIterRecord table1) Empty (LoopIterRecord newTable) value = LoopIterRecord (DMap.insert value (subTable1) newTable)
			where
				subTable1 = DMap.findWithDefault Empty value table1
joinTupleTable_foldl Empty (LoopIterRecord table2) (LoopIterRecord newTable) value = LoopIterRecord (DMap.insert value (subTable2) newTable)
			where
				subTable2 = DMap.findWithDefault Empty value table2
joinTupleTable_foldl Empty Empty (LoopIterRecord newTable) value = LoopIterRecord (DMap.insert value Empty newTable)


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