module PreProcessor where

--	Simple preprocessor used to circumvent some issues with Language-Fortran parser.

import Data.Char

preProcess :: Bool -> String -> String
preProcess False inputStr = removeBlankLines $ andOperatorFix $ orOperatorFix $ containsStatementFix $ caseStatementFix $ inputStr
preProcess True inputStr = removeBlankLines $ andOperatorFix $ orOperatorFix $ containsStatementFix $ caseStatementFix $ fixedForm $ inputStr

caseInsensitive_strReplace :: [Char] -> [Char] -> [Char] -> [Char]
caseInsensitive_strReplace original replace str 	
										| take (length original) (map (toLower) str) == original 	
														= replace ++ caseInsensitive_strReplace original replace (drop (length original) str)
										| str == []		= []
										| otherwise 	= (take 1 str) ++ caseInsensitive_strReplace original replace (drop 1 str)

removeBlankLines :: String -> String
removeBlankLines inputStr = foldl (removeBlankLines_foldl) "" allLines
		where
			allLines = lines inputStr

removeBlankLines_foldl :: String -> String -> String
removeBlankLines_foldl accum item = accum ++ (if all (isSpace) item then "" else item ++ "\n")

-- removeBlankLines :: [Char] -> [Char]
-- removeBlankLines [] = []
-- removeBlankLines ('\n':'\n':str) = removeBlankLines ('\n':str)
-- removeBlankLines (char:str) = char:(removeBlankLines str)

caseStatementFix :: String -> String
caseStatementFix input = caseInsensitive_strReplace "\ncase(" "\n case(" (caseInsensitive_strReplace "\ncase " "\n case " input)

containsStatementFix :: String -> String
containsStatementFix input = (caseInsensitive_strReplace "\ncontains" "\n contains " (caseInsensitive_strReplace "\ncontains " "\n contains " input))

orOperatorFix :: String -> String
orOperatorFix input = caseInsensitive_strReplace ".or." " .or. " input

andOperatorFix :: String -> String
andOperatorFix input = caseInsensitive_strReplace ".and." " .and. " input

semiColonFix :: String -> String
semiColonFix input = caseInsensitive_strReplace ";" "\n" input

fixedForm :: String -> String
fixedForm  inputStr = foldl (\accum item -> accum ++ (take 72 item) ++ "\n") "" allLines
		where
			allLines = lines inputStr
