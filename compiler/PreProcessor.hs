module PreProcessor where

import Data.Char

preProcess :: String -> String
preProcess inputStr = orOperatorFix $ containsStatementFix $ caseStatementFix $ inputStr
--preProcess inputStr = (show $ ((head inputStr) == '\n')) ++ "\n" ++ inputStr
--preProcess inputStr = continuationFix $ inputStr

caseInsensitive_strReplace :: [Char] -> [Char] -> [Char] -> [Char]
caseInsensitive_strReplace original replace str 	
										| take (length original) (map (toLower) str) == original 	
														= replace ++ caseInsensitive_strReplace original replace (drop (length original) str)
										| str == []		= []
										| otherwise 	= (take 1 str) ++ caseInsensitive_strReplace original replace (drop 1 str)

caseStatementFix :: String -> String
caseStatementFix input = caseInsensitive_strReplace "\ncase(" "\n case(" (caseInsensitive_strReplace "\ncase " "\n case " input)

containsStatementFix :: String -> String
containsStatementFix input =  caseInsensitive_strReplace "\ncontains\n" "\n contains " (caseInsensitive_strReplace "\ncontains " "\n contains " input)

orOperatorFix :: String -> String
orOperatorFix input = caseInsensitive_strReplace ".or." " .or. " input