module Transformer where

import Data.Char
-- Generally will take a string argument and transform it
-- A majority of these transformations can be achieved with built-in functions
-- However more niche ones will require normal + recursive worker functions


----- basic -----
-- reverse
-- make all caps
-- make lowercase

reverseText :: String -> String
reverseText [] = ""
reverseText input_s = reverse input_s

capitalizeText :: String -> String
capitalizeText input_s = capitalizeTextWorker input_s

capitalizeTextWorker :: String -> String
capitalizeTextWorker [] = ""
capitalizeTextWorker input_s = if isUpper (head input_s)
 then ([(head input_s)] ++ (capitalizeTextWorker (tail(input_s))))
 else ([(toUpper(head input_s))] ++ (capitalizeTextWorker (tail(input_s))))

lowercaseText :: String -> String
lowercaseText input_s = lowercaseTextWorker input_s

lowercaseTextWorker:: String -> String
lowercaseTextWorker [] = ""
lowercaseTextWorker input_s = if isLower (head input_s)
 then ([(head input_s)] ++ (lowercaseTextWorker (tail(input_s))))
 else ([(toLower(head input_s))] ++ (lowercaseTextWorker (tail(input_s))))

removeCharFromText :: String -> Char -> String --input char, input string, final text
removeCharFromText input_s input_c = filter (/= input_c) input_s

removeCharsFromText :: String -> String -> String --PLURAL!!!
removeCharsFromText input_s input_chars = [ c | c <- input_s, not (c `elem` input_chars)]

removeAllButInputFromText :: String -> String -> String -- more efficient to use in some cases
removeAllButInputFromText input_s input_chars = [ c | c <- input_s, c `elem` input_chars]


--remove specific characters
--  Remove letters
--  Remove spaces
--  Remove Punctuation
--  Remove letters or Numbers
--Replace specific letters with another


removeVowelsFromText :: String -> String
removeVowelsFromText input_s = (removeCharsFromText input_s "aeiouAEIOU")

removeSpacesFromText :: String -> String
removeSpacesFromText input_s = (removeCharFromText input_s ' ')

removeLettersFromText :: String -> String
removeLettersFromText input_s = (removeCharsFromText input_s (['A'..'Z'] ++ ['a'..'z']))

removeNonLetters :: String -> String
removeNonLetters input_s = removeAllButInputFromText input_s (['A'..'Z'] ++ ['a'..'z'])

removeSymbolsFromText :: String -> String
removeSymbolsFromText input_s = removeAllButInputFromText input_s (['A'..'Z'] ++ ['a'..'z'] ++ [' '])

removeNonSymbolsFromText :: String -> String
removeNonSymbolsFromText input_s = removeCharsFromText input_s (['A'..'Z'] ++ ['a'..'z'] ++ [' '])
