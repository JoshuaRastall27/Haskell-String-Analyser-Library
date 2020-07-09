module Analyzer where


-- slower to do as it has to analyse the entire string for the input character
-- Could perform analyse on each character as it's passed in with if statements
-- More readable and reusable as seperate functions though
-- While the text file will need to be constantly iterated, the code will be a lot more modular
-- Considered doing a function which would apply every check for each inputted character
-- But would require messy if statements and global variables to keep track of everything
-- Will not be able to be used for anything else but this project

-- (Use show to convert int to string     e.g    putStrLn ()"this:" ++ (show 2))
-- have the smaller string on the left of ++
-- go through all

analyzeStringSimpleOutput :: String -> String
analyzeStringSimpleOutput input_s = do
  "--- String Analysis Results ---:\n" ++ (analyzeStringSimpleGetResultString input_s) ++ "\n" ++ "Input text:\n <" ++ input_s ++">"

-- dirty concating code, find  way to do in neater paragraphs
analyzeStringSimpleGetResultString :: String -> String
analyzeStringSimpleGetResultString [] = "Error, no input textfile"
analyzeStringSimpleGetResultString input_s = do
  "Character Count:"++ show(calcTotalCharCount input_s) ++ "\n\n" ++ (getStringLetterDetails input_s) ++ "\n" ++ (getStringPunctDetails input_s)



-- Functions to get string details


getStringLetterDetails :: String -> String
getStringLetterDetails input_s = do
  "Letter Count:"++ show(calcTotalLetterCount input_s) ++ "\n"++ "    Uppercase:"++ show(calcTotalUppercaseLetterCount input_s) ++ "\n"++ "    Lowercase:"++ show(calcTotalLowercaseLetterCount input_s) ++ "\n"++ "    Vowels:"++ show(calcTotalVowelCount input_s) ++ "\n"++ "    Consonants:"++ show(calcTotalConsonantCount input_s) ++ "\n"

getStringPunctDetails :: String -> String
getStringPunctDetails input_s = do
  "Punctuation count:" ++ show(calcTotalPunctCount input_s) ++ "\n"++ "    Spaces:"++ show(calcTotalSpaceCount input_s) ++ "\n"++ "    Symbols:"++ show(calcTotalPunctSymbolCount input_s) ++ "\n"

getStringDigitDetails :: String -> String
getStringDigitDetails input_s = do
  "Digit Count:" ++ show(calcTotalDigitCount input_s)


-- simple length counter
calcTotalCharCount :: String -> Int
calcTotalCharCount [] = 0
calcTotalCharCount input_s = length input_s

-- /= filters out characters,  == gets out only those chars
calcSpecificCharCount :: Char -> String -> Int
calcSpecificCharCount input_c [] = 0 --end of string. End recursion
calcSpecificCharCount input_c input_s = length (filter (==input_c) input_s)

--PLURAL - put in a string and it goes through each character in the string
-- Can be used in place of non-plural, may be redundant, but a lot of cases require
calcSpecificCharsCount :: String -> String -> Int --target string, character string, count
calcSpecificCharsCount t_string [] = 0--until all characters have been gone through
calcSpecificCharsCount t_string c_string = length(filter (==(head c_string)) t_string) + calcSpecificCharsCount t_string (tail c_string)



-- Cannot work out how to do it higher order, brute force FOR NOW
-- will need to iterate manually throughout every character for each 'analysis session'

--Will calc the total letter count
--Works by concating a list of all english letters (a - z A - Z)
--Filters out the string for every character recursively in the work function
--every recursion takes off the head of the sought char list

calcTotalUppercaseLetterCount :: String -> Int
calcTotalUppercaseLetterCount [] = 0
calcTotalUppercaseLetterCount input_s = calcSpecificCharsCount input_s ['A'..'Z']

calcTotalLowercaseLetterCount :: String -> Int
calcTotalLowercaseLetterCount [] = 0
calcTotalLowercaseLetterCount input_s = calcSpecificCharsCount input_s ['a'..'z']


calcTotalLetterCount :: String -> Int --excludes spaces and puncuation
calcTotalLetterCount [] = 0
calcTotalLetterCount input_s = (calcTotalUppercaseLetterCount input_s) + (calcTotalLowercaseLetterCount input_s)


--deprecetated, replaced by combining smaller modular functions which seperatly get upper + lowercase.
--calcTotalLetterCount :: String -> Int --excludes spaces and puncuation
--calcTotalLetterCount [] = 0
--calcTotalLetterCount input_s = calcSpecificCharsCount input_s (['a'..'z'] ++ ['A'..'Z'])

--Uses previously made function to search for vowels specifically (aeiou) (both uppercase and lowercase) (y is considered constanant here)
-- ['a','e','i','o','u'] instead of "aeiou" for readability, same result either way

calcTotalDigitCount :: String -> Int
calcTotalDigitCount [] = 0
calcTotalDigitCount input_s = calcSpecificCharsCount input_s (['0'..'9'])

calcTotalVowelCount :: String -> Int
calcTotalVowelCount [] = 0
calcTotalVowelCount input_s = calcSpecificCharsCount input_s (['a','e','i','o','u'] ++ ['A','E','I','O','U']) --start the recursive worker

-----important notes for following functions-----
-- Some values (punctuation count, consonant count ) can be simply calculated with logic and the already implemented functions
-- While this will create overhead as more lines of code will be run to get the values to calucate with
-- It reduced the amount of required maintainable functions
-- as long as the foundation functions do not have errors, and the logic is sound.
-- This extra required processing will not cause much issues as the project itself is smaller scale and will be handling text files
-- This implementation is catered towards the english language, other languages would require a seperate program due to different language rules
-- It has been considered and i may implement proper search algorithms as 'proof of concept', but it works for now

-- Can be lazy about consonants. Can just do letters minus vowels
-- logic: letters - vowels = consonants
-- Slighty more more overhead, but more readable, logical and straightforward than throwing in [b,c,d,f...] + [B,C,D,F...]
calcTotalConsonantCount :: String -> Int
calcTotalConsonantCount [] = 0
calcTotalConsonantCount input_s = ((calcTotalLetterCount input_s) - (calcTotalVowelCount input_s))

--can also be lazy for punctuation
-- logic: total characters - letters -- numbers = punctuation char count  (spaces count as puncuation apparently)
--same overhead situation as the consonant, but works for the scale of this project
calcTotalPunctCount :: String -> Int
calcTotalPunctCount  [] = 0
calcTotalPunctCount input_s = ((calcTotalCharCount input_s) - (calcTotalLetterCount input_s) - (calcTotalDigitCount input_s))

calcTotalSpaceCount :: String -> Int
calcTotalSpaceCount [] = 0
calcTotalSpaceCount  input_s = calcSpecificCharCount ' ' input_s

-- same as before, but minus spaces aswell- Just to get the symbols like: !,.'[] etc  for more advanced analysis
-- Bit to many symbols to get all of them, a lot simplier to apply logic
-- logic: total chars - letters - spaces - numbers   = punctuation char count
calcTotalPunctSymbolCount :: String -> Int
calcTotalPunctSymbolCount  [] = 0
calcTotalPunctSymbolCount input_s = ((calcTotalCharCount input_s) - (calcTotalLetterCount input_s) - (calcTotalSpaceCount input_s) - (calcTotalDigitCount input_s))


--Depreceated manual character counter before i discovered the filter functionality
--countCharactersInString :: Char -> String -> Int
--countCharactersInString input_c [] = 0 --end of string. End recursion
--countCharactersInString input_c s = do
--{
--if input_c == (head s) then 1 + (countCharactersInString input_c (tail s))
--else 0 + (countCharactersInString input_c (tail s))
--}
