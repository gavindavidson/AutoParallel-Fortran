module PreParser where

import Data.Char

preParse :: String -> String
preParse inputStr = orOperatorFix $ containsStatementFix $ caseStatementFix $ inputStr

listReplace :: Eq a => [a] -> [a] -> [a] -> [a]
listReplace original replace list 	| take (length original) list == original 	= replace ++ listReplace original replace (drop (length original) list)
									| list == []								= []
									| otherwise 								= (take 1 list) ++ listReplace original replace (drop 1 list)

caseStatementFix :: String -> String
caseStatementFix input = listReplace "case" " case" input

containsStatementFix :: String -> String
containsStatementFix input = listReplace "contains" " contains" input

orOperatorFix :: String -> String
orOperatorFix input = listReplace ".or." " .or. " input

