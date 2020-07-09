

import Test.HUnit
import Test.QuickCheck
import Analyzer
import Transformer

------------------------------------
----------Property Testing----------
------------------------------------

-- check that the more specific aspects of the letters add up to the main contents
-- Total letters == uppercase + lowercase == Vowels + Consonants
run_Prop_Tests :: IO()
run_Prop_Tests = do
  putStrLn "\nTotal character Count tests"
  quickCheck prop_Total_Counts

  putStrLn "\nLetter Count Test"
  quickCheck prop_Letter_Aspects

  putStrLn "\nPunctuation Count Test"
  quickCheck prop_Punct_Count


  putStrLn "\n\nLetter removal Test 1"
  quickCheck prop_Letter_removal_1

  putStrLn "\n\nLetter removal Test 2"
  quickCheck prop_Letter_removal_2


  putStrLn "\nAll tests run"

prop_Total_Counts :: String -> Bool
prop_Total_Counts input_s = (calcTotalCharCount input_s == ((calcTotalLetterCount input_s) + (calcTotalPunctCount input_s) + (calcTotalDigitCount input_s)))

prop_Letter_Aspects :: String -> Bool
prop_Letter_Aspects input_s = (((calcTotalLetterCount input_s) == ((calcTotalUppercaseLetterCount input_s) + (calcTotalLowercaseLetterCount input_s))) && ((calcTotalLetterCount input_s) == ((calcTotalVowelCount input_s) + (calcTotalConsonantCount input_s))))

prop_Punct_Count :: String -> Bool
prop_Punct_Count input_s = (calcTotalPunctCount input_s == ((calcTotalSpaceCount input_s) + (calcTotalPunctSymbolCount input_s)))

-- use functions to check if transformations have been applied.

prop_Letter_removal_1 :: String -> Bool
prop_Letter_removal_1 input_s = (calcTotalLetterCount (removeLettersFromText input_s)) == 0

prop_Letter_removal_2 :: String -> Bool
prop_Letter_removal_2 input_s = ((length (removeNonLetters input_s)) == (calcTotalLetterCount input_s))


--calcTotalCharCount
--calcTotalUppercaseLetterCount
--calcTotalLowercaseLetterCount
--calcTotalLetterCount
--calcTotalDigitCount
--calcTotalVowelCount
--calcTotalConsonantCount
--calcTotalPunctCount
--calcTotalSpaceCount
--calcTotalPunctSymbolCount


-------------------------------------------------------------------------------------------------------------
---------------------------------------------- Unit Testing --------------------------------------------------
-------------------------------------------------------------------------------------------------------------


-- runTestTT tests

-- total chars
-- specific char
-- specific char's
----------------------------------------------------------------------------------
------------------------------------ Analyzer ------------------------------------
----------------------------------------------------------------------------------
t_total_char_1 = TestCase (assertEqual "for calcTotalCharCount(Short Text) = 13" 13 (calcTotalCharCount "ThisIs13chars"))
t_total_char_2 = TestCase (assertEqual "for calcTotalCharCount(Longer text) = 89" 89 (calcTotalCharCount "For a better and more reliable test, a larger and even longer input string is required!!!"))

t_total_char_tests = TestList [TestLabel "t_total_char_1" t_total_char_1,
                               TestLabel "t_total_char_2" t_total_char_2]

t_specific_char_1 = TestCase (assertEqual "for calcSpecificCharCount('a' 'aAaAaAaAa') = 5" 5 (calcSpecificCharCount 'a' "aAaAaAaAa"))
t_specific_char_2 = TestCase (assertEqual "for calcSpecificCharCount('a' 'AAAAA') = 0" 0 (calcSpecificCharCount 'a' "AAAAA"))
t_specific_char_3 = TestCase (assertEqual "for calcSpecificCharCount('A' 'aAaAaAaAa') = 4" 4 (calcSpecificCharCount 'A' "aAaAaAaAa"))
t_specific_char_4 = TestCase (assertEqual "for calcSpecificCharCount('e' 'The perfect test material') = 5" 5 (calcSpecificCharCount 'e' "The perfect test material"))
t_specific_char_5 = TestCase (assertEqual "for calcSpecificCharCount(' ' 'How Many Spaces Are there?') = 4" 4 (calcSpecificCharCount ' ' "How Many Spaces Are there?"))
t_specific_char_6 = TestCase (assertEqual "for calcSpecificCharCount('!' '!Punctuation!!!AlSo!!!Works!') = 8" 8 (calcSpecificCharCount '!' "!Punctuation!!!AlSo!!!Works!"))

t_specific_char_tests = TestList [TestLabel "t_specific_char_1" t_specific_char_1,
                               TestLabel "t_specific_char_2" t_specific_char_2,
                               TestLabel "t_specific_char_3" t_specific_char_3,
                               TestLabel "t_specific_char_4" t_specific_char_4,
                               TestLabel "t_specific_char_5" t_specific_char_5,
                               TestLabel "t_specific_char_6" t_specific_char_6]

t_specific_chars_1 = TestCase (assertEqual "for calcSpecificChar(s)Count('a' 'aAaAaAaAa') = 5" 5 (calcSpecificCharsCount "a" "aAaAaAaAa")) --put single char string into it
t_specific_chars_2 = TestCase (assertEqual "for calcSpecificChar(s)Count('aA' 'aAaAaAaAa') = 9" 9 (calcSpecificCharsCount "aA" "aAaAaAaAa")) -- search lowercase + uppercase
t_specific_chars_3 = TestCase (assertEqual "for calcSpecificChar(s)Count('The' 'The Test') = 5" 5 (calcSpecificCharsCount "The" "The Test")) --DOES NOT SEARCH FOR WORDS, JUST the individual chars
t_specific_chars_4 = TestCase (assertEqual "for calcSpecificChar(s)Count('eTh' 'The Test') = 5" 5 (calcSpecificCharsCount "eTh" "The Test"))

t_specific_chars_tests = TestList [TestLabel "t_specific_chars_1" t_specific_chars_1,
                               TestLabel "t_specific_chars_2" t_specific_chars_2,
                               TestLabel "t_specific_chars_3" t_specific_chars_3,
                               TestLabel "t_specific_chars_4" t_specific_chars_4]

--calcTotalUppercaseLetterCount :: String -> Int
t_Calc_Uppercase_Count_1 = TestCase (assertEqual "for calcTotalUppercaseLetterCount('My StRiNg') = 4" 4 (calcTotalUppercaseLetterCount "My StRiNg"))
t_Calc_Uppercase_Count_2 = TestCase (assertEqual "for calcTotalUppercaseLetterCount('') = 0" 0 (calcTotalUppercaseLetterCount ""))
t_Calc_Uppercase_Count_3 = TestCase (assertEqual "for calcTotalUppercaseLetterCount('MY STRING') = 8" 8 (calcTotalUppercaseLetterCount "MY STRING"))
t_Calc_Uppercase_Count_4 = TestCase (assertEqual "for calcTotalUppercaseLetterCount('my string') = 0" 0 (calcTotalUppercaseLetterCount "my string"))

t_calc_Uppercase_Count_tests = TestList [TestLabel "t_Calc_Uppercase_Count_1" t_Calc_Uppercase_Count_1,
                               TestLabel "t_Calc_Uppercase_Count_2" t_Calc_Uppercase_Count_2,
                               TestLabel "t_Calc_Uppercase_Count_3" t_Calc_Uppercase_Count_3,
                               TestLabel "t_Calc_Uppercase_Count_4" t_Calc_Uppercase_Count_4]
--calcTotalLowercaseLetterCount :: String -> Int
t_Calc_Lowercase_Count_1 = TestCase (assertEqual "for calcTotalLowercaseLetterCount('My StRiNg') = 4" 4 (calcTotalLowercaseLetterCount "My StRiNg"))
t_Calc_Lowercase_Count_2 = TestCase (assertEqual "for calcTotalLowercaseLetterCount('') = 0" 0 (calcTotalLowercaseLetterCount ""))
t_Calc_Lowercase_Count_3 = TestCase (assertEqual "for calcTotalLowercaseLetterCount('MY STRING') = 0" 0 (calcTotalLowercaseLetterCount "MY STRING"))
t_Calc_Lowercase_Count_4 = TestCase (assertEqual "for calcTotalLowercaseLetterCount('my string') = 8" 8 (calcTotalLowercaseLetterCount "my string"))

t_calc_Lowercase_Count_tests = TestList [TestLabel "t_Calc_Lowercase_Count_1" t_Calc_Lowercase_Count_1,
                               TestLabel "t_Calc_Lowercase_Count_2" t_Calc_Lowercase_Count_2,
                               TestLabel "t_Calc_Lowercase_Count_3" t_Calc_Lowercase_Count_3,
                               TestLabel "t_Calc_Lowercase_Count_4" t_Calc_Lowercase_Count_4]


--calcTotalLetterCount :: String -> Int --excludes spaces and puncuation
t_calc_Letter_Count_1 = TestCase (assertEqual "For calcTotalLetterCount('There Are 29 Letters in this Sentence') = 29" 29 (calcTotalLetterCount "There Are 28 Letters in this Sentence"))
t_calc_Letter_Count_2 = TestCase (assertEqual "For calcTotalLetterCount('1 2 3 4 5 6 7 8 9 10') = 0" 0 (calcTotalLetterCount "1 2 3 4 5 6 7 8 9 10"))
t_calc_Letter_Count_3 = TestCase (assertEqual "For calcTotalLetterCount('1ab243s94ew') = 5" 5 (calcTotalLetterCount "1ab243s94ew"))

t_calc_Letter_Count_tests = TestList [TestLabel "t_calc_Letter_Count_1" t_calc_Letter_Count_1,
                               TestLabel "t_calc_Letter_Count_2" t_calc_Letter_Count_2,
                               TestLabel "t_calc_Letter_Count_3" t_calc_Letter_Count_3]



--calcTotalDigitCount :: String -> Int
t_calcTotalDigitCount_1 = TestCase (assertEqual "For calcTotalDigitCount('1') = 1" 1 (calcTotalDigitCount "1"))
t_calcTotalDigitCount_2 = TestCase (assertEqual "For calcTotalDigitCount('123') = 3" 3 (calcTotalDigitCount "123"))
t_calcTotalDigitCount_3 = TestCase (assertEqual "For calcTotalDigitCount('1bh23ds456d7uu89') = 9" 9 (calcTotalDigitCount "1bh23ds456d7uu89"))
t_calcTotalDigitCount_4 = TestCase (assertEqual "For calcTotalDigitCount('bhdsduu') = 0" 0 (calcTotalDigitCount "bhdsduu"))


-- calcTotalVowelCount :: String -> Int AEIOU
t_calc_Vowel_Count_1 = TestCase (assertEqual "For calcVowelLetterCount('AEIOU') = 5" 5 (calcTotalVowelCount "AEIOU"))
t_calc_Vowel_Count_2 = TestCase (assertEqual "For calcVowelLetterCount('aeiou') = 5" 5 (calcTotalVowelCount "aeiou"))
t_calc_Vowel_Count_3 = TestCase (assertEqual "For calcVowelLetterCount('aEiOu') = 5" 5 (calcTotalVowelCount "aEiOu"))
t_calc_Vowel_Count_4 = TestCase (assertEqual "For calcVowelLetterCount('My String') = 1" 1 (calcTotalVowelCount "My String"))

t_calc_Vowel_Count_tests = TestList [TestLabel "t_calc_Vowel_Count_1" t_calc_Vowel_Count_1,
                               TestLabel "t_calc_Vowel_Count_2" t_calc_Vowel_Count_2,
                               TestLabel "t_calc_Vowel_Count_3" t_calc_Vowel_Count_3,
                               TestLabel "t_calc_Vowel_Count_4" t_calc_Vowel_Count_4]


-- calcTotalConsonantCount :: String -> Int AEIOU
t_calc_Consonant_Count_1 = TestCase (assertEqual "For calcConsonantLetterCount('AEIOU') = 0" 0 (calcTotalConsonantCount "AEIOU"))
t_calc_Consonant_Count_2 = TestCase (assertEqual "For calcConsonantLetterCount('bCdFg') = 5" 5 (calcTotalConsonantCount "bCdFg"))
t_calc_Consonant_Count_3 = TestCase (assertEqual "For calcConsonantLetterCount('') = 0" 0 (calcTotalConsonantCount ""))
t_calc_Consonant_Count_4 = TestCase (assertEqual "For calcConsonantLetterCount('My String') = 7" 7 (calcTotalConsonantCount "My String"))


t_calc_Consonant_Count_tests = TestList [TestLabel "t_calc_Consonant_Count_1" t_calc_Consonant_Count_1,
                               TestLabel "t_calc_Consonant_Count_2" t_calc_Consonant_Count_2,
                               TestLabel "t_calc_Consonant_Count_3" t_calc_Consonant_Count_3,
                               TestLabel "t_calc_Consonant_Count_4" t_calc_Consonant_Count_4]

--calcTotalPunctCount :: String -> Int
t_calc_total_Punct_Count_1 = TestCase (assertEqual "For calcTotalPunctCount('    ') = 4" 4 (calcTotalPunctCount "    "))
t_calc_total_Punct_Count_2 = TestCase (assertEqual "For calcTotalPunctCount('!?:') = 3" 3 (calcTotalPunctCount "!?:"))
t_calc_total_Punct_Count_3 = TestCase (assertEqual "For calcTotalPunctCount('ASTRING') = 0" 0 (calcTotalPunctCount "ASTRING"))
t_calc_total_Punct_Count_4 = TestCase (assertEqual "For calcTotalPunctCount('A!STRING') = 1" 1 (calcTotalPunctCount "A!STRING"))
t_calc_total_Punct_Count_5 = TestCase (assertEqual "For calcTotalPunctCount('A STRING') = 1" 1 (calcTotalPunctCount "A STRING"))

t_calc_total_Punct_Count_tests = TestList [TestLabel "t_calc_Punct_Count_1" t_calc_total_Punct_Count_1,
                               TestLabel "t_calc_total_Punct_Count_2" t_calc_total_Punct_Count_2,
                               TestLabel "t_calc_total_Punct_Count_3" t_calc_total_Punct_Count_3,
                               TestLabel "t_calc_total_Punct_Count_4" t_calc_total_Punct_Count_4,
                               TestLabel "t_calc_total_Punct_Count_5" t_calc_total_Punct_Count_5]

--calcTotalSpaceCount :: String -> Int
t_calc_total_Space_Count_1 = TestCase (assertEqual "For calcTotalSpaceCount(' ') = 1" 1 (calcTotalSpaceCount " "))
t_calc_total_Space_Count_2 = TestCase (assertEqual "For calcTotalSpaceCount('   ') = 3" 3 (calcTotalSpaceCount "   "))
t_calc_total_Space_Count_3 = TestCase (assertEqual "For calcTotalSpaceCount('') = 0" 0 (calcTotalSpaceCount ""))
t_calc_total_Space_Count_4 = TestCase (assertEqual "For calcTotalSpaceCount('This is single Spaced') = 3" 3 (calcTotalSpaceCount "This is single Spaced"))
t_calc_total_Space_Count_5 = TestCase (assertEqual "For calcTotalSpaceCount('This  is  Double  Spaced') = 6" 6 (calcTotalSpaceCount "This  is  Double  Spaced"))
t_calc_total_Space_Count_6 = TestCase (assertEqual "For calcTotalSpaceCount('ThisHasNoSpaces') = 0" 0 (calcTotalSpaceCount "ThisHasNoSpaces"))
t_calc_total_Space_Count_7 = TestCase (assertEqual "For calcTotalSpaceCount('!?>') = 0" 0 (calcTotalSpaceCount "!?>"))
t_calc_total_Space_Count_8 = TestCase (assertEqual "For calcTotalSpaceCount('! ? >') = 2" 2 (calcTotalSpaceCount "! ? >"))

t_calc_total_Space_Count_tests = TestList[TestLabel "t_calc_total_Space_Count_1" t_calc_total_Space_Count_1,
                                          TestLabel "t_calc_total_Space_Count_2" t_calc_total_Space_Count_2,
                                          TestLabel "t_calc_total_Space_Count_3" t_calc_total_Space_Count_3,
                                          TestLabel "t_calc_total_Space_Count_4" t_calc_total_Space_Count_4,
                                          TestLabel "t_calc_total_Space_Count_5" t_calc_total_Space_Count_5,
                                          TestLabel "t_calc_total_Space_Count_6" t_calc_total_Space_Count_6,
                                          TestLabel "t_calc_total_Space_Count_7" t_calc_total_Space_Count_7,
                                          TestLabel "t_calc_total_Space_Count_8" t_calc_total_Space_Count_8]


--calcTotalPunctSymbolCount :: String -> Int

t_calcTotalPunctSymbolCount_1 = TestCase (assertEqual "For calcTotalSpaceCount('as,<w3a,sd!@d') = 5" 5 (calcTotalPunctSymbolCount ",<,!@"))
t_calcTotalPunctSymbolCount_2 = TestCase (assertEqual "For calcTotalSpaceCount('as,< w 3 a , s d ! @ d') = 5" 5 (calcTotalPunctSymbolCount "a s w a s d d"))



-------------------------------------------------------------------------------------
------------------------------------ Transformer ------------------------------------
-------------------------------------------------------------------------------------

-- runTestTT tests

t_trans_reverse_1 = TestCase (assertEqual "For reverseText('My String' = 'gnirtS yM')" "gnirtS yM" (reverseText "My String"))
t_trans_reverse_2 = TestCase (assertEqual "For reverseText('12345' = '54321')" "54321" (reverseText "12345"))
t_trans_reverse_3 = TestCase (assertEqual "For reverseText('' = '')" "" (reverseText ""))

t_capitalizeText_1 = TestCase (assertEqual "For capitalizeText('m' = 'M')" "m" (capitalizeText "M"))
t_capitalizeText_2 = TestCase (assertEqual "For capitalizeText('M' = 'M')" "M" (capitalizeText "M"))
t_capitalizeText_3 = TestCase (assertEqual "For capitalizeText(' ' = ' ')" " " (capitalizeText " "))
t_capitalizeText_4 = TestCase (assertEqual "For capitalizeText('' = '')" "" (capitalizeText ""))
t_capitalizeText_5 = TestCase (assertEqual "For capitalizeText('My String' = 'MY STRING')" "MY STRING" (capitalizeText "My String"))

t_trans_capitalize_Text_Tests = TestList[TestLabel "t_capitalizeText_1" t_capitalizeText_1,
                                          TestLabel "t_capitalizeText_2" t_capitalizeText_2,
                                          TestLabel "t_capitalizeText_3" t_capitalizeText_3,
                                          TestLabel "t_capitalizeText_4" t_capitalizeText_4,
                                          TestLabel "t_capitalizeText_5" t_capitalizeText_5]

t_lowercaseText_1 = TestCase (assertEqual "For lowercaseText('M' = 'm')" "m" (lowercaseText "M"))
t_lowercaseText_2 = TestCase (assertEqual "For lowercaseText('M' = 'm')" "m" (lowercaseText "M"))
t_lowercaseText_3 = TestCase (assertEqual "For lowercaseText(' ' = ' ')" " " (lowercaseText " ")) --empty checks , need before iterating on entire strings
t_lowercaseText_4 = TestCase (assertEqual "For lowercaseText('' = '')" "" (lowercaseText ""))
t_lowercaseText_5 = TestCase (assertEqual "For lowercaseText('My String' = 'my string')" "my string" (lowercaseText "My String"))
t_lowercaseText_6 = TestCase (assertEqual "For lowercaseText('MY STRING' = 'my string')" "my string" (lowercaseText "MY STRING"))

t_trans_LowercaseText_tests = TestList[TestLabel "t_lowercaseText_1" t_lowercaseText_1,
                                          TestLabel "t_lowercaseText_2" t_lowercaseText_2,
                                          TestLabel "t_lowercaseText_3" t_lowercaseText_3,
                                          TestLabel "t_lowercaseText_4" t_lowercaseText_4,
                                          TestLabel "t_lowercaseText_5" t_lowercaseText_5,
                                          TestLabel "t_lowercaseText_6" t_lowercaseText_6]


-- removeCharFromText :: String -> Char -> String
t_removeCharFromText_1 = TestCase(assertEqual "For removeCharFromText('ababababa' 'a')" "bbbb" (removeCharFromText "ababababa" 'a'))
t_removeCharFromText_2 = TestCase(assertEqual "For removeCharFromText('aaa' 'a')" "" (removeCharFromText "aaa" 'a'))
t_removeCharFromText_3 = TestCase(assertEqual "For removeCharFromText(' a a a ' ' ')" "aaa" (removeCharFromText" a a a " ' '))

t_trans_removeCharFromText_tests = TestList[TestLabel "t_removeCharFromText_1" t_removeCharFromText_1,
                                          TestLabel "t_removeCharFromText_2" t_removeCharFromText_2,
                                          TestLabel "t_removeCharFromText_3" t_removeCharFromText_3]


-- removeCharsFromText :: String -> String -> String
t_removeCharsFromText_1 = TestCase(assertEqual "For removeCharsFromText('abcabcabcabc' 'a')" "bcbcbcbc" (removeCharsFromText "abcabcabcabc" "a"))
t_removeCharsFromText_2 = TestCase(assertEqual "For removeCharsFromText('abcabcabcabc' 'a')" "cccc" (removeCharsFromText "abcabcabcabc" "ab"))
t_removeCharsFromText_3 = TestCase(assertEqual "For removeCharsFromText('abcabcabcabc' 'a')" "bbbb" (removeCharsFromText "abcabcabcabc" "ac"))
t_removeCharsFromText_4 = TestCase(assertEqual "For removeCharsFromText('abcabcabcabc' 'a')" "" (removeCharsFromText "abcabcabcabc" "abc"))

t_trans_removeCharsFromText_tests = TestList[TestLabel "t_removeCharsFromText_1" t_removeCharsFromText_1,
                                          TestLabel "t_removeCharsFromText_2" t_removeCharsFromText_2,
                                          TestLabel "t_removeCharsFromText_3" t_removeCharsFromText_3,
                                          TestLabel "t_removeCharsFromText_4" t_removeCharsFromText_4]

--removeAllButInputFromText :: String -> String -> String
--removeAllButInputFromText input_s input_chars

t_removeAllButInputFromText_1 = TestCase(assertEqual "For removeAllButInputFromText('abcabcabc' 'a')" "aaa" (removeAllButInputFromText "abcabcabc" "a"))
t_removeAllButInputFromText_2 = TestCase(assertEqual "For removeAllButInputFromText('abcabcabc' 'ab')" "ababab" (removeAllButInputFromText "abcabcabc" "ab"))
t_removeAllButInputFromText_3 = TestCase(assertEqual "For removeAllButInputFromText('abcabcabc' 'a')" "abcabcabc" (removeAllButInputFromText "abcabcabc" "abc"))
t_removeAllButInputFromText_4 = TestCase(assertEqual "For removeAllButInputFromText('abcabcabc' 'a')" "aaa" (removeAllButInputFromText "abcabcabc" "ad"))


--removeLettersFromText String -> String

t_removeLettersFromText_1 = TestCase(assertEqual "For removeLettersFromText('this is a test string!')" "    !" (removeLettersFromText "this is a test string!"))
t_removeLettersFromText_2 = TestCase(assertEqual "For removeLettersFromText('this!is!a!test!string!')" "!!!!!" (removeLettersFromText "this!is!a!test!string!"))
t_removeLettersFromText_3 = TestCase(assertEqual "For removeLettersFromText('THIS!IS!A!TEST!STRING!')" "!!!!!" (removeLettersFromText "THIS!IS!A!TEST!STRING!"))
t_removeLettersFromText_4 = TestCase(assertEqual "For removeLettersFromText('!!!!!')" "!!!!!" (removeLettersFromText "!!!!!"))

-- removeNonLetters :: String -> String
t_removeNonLetters_1 = TestCase(assertEqual "for removeNonLetters('!!!This is a test string!!!')" "Thisisateststring" (removeNonLetters "!!!This is a test string!!!"))
t_removeNonLetters_2 = TestCase(assertEqual "for removeNonLetters('This is a test string')" "Thisisateststring" (removeNonLetters "!!!This is a test string!!!"))

-- removeVowelsFromText
t_removeVowelsFromText_1 = TestCase(assertEqual "for removeVowelsFromText('this is a test string')" "ths s  tst strng" (removeVowelsFromText "this is a test string"))
t_removeVowelsFromText_2 = TestCase(assertEqual "for removeVowelsFromText('THIS IS A TEST STRING')" "THS S  TST STRNG" (removeVowelsFromText "THIS IS A TEST STRING"))
t_removeVowelsFromText_3 = TestCase(assertEqual "for removeVowelsFromText('ths s tst strng')" "ths s tst strng" (removeVowelsFromText "ths s tst strng"))

-- removeSpacesFromText
t_removeSpacesFromText_1 = TestCase(assertEqual "for removeSpacesFromText('This is a test string')" "This is a test string" (removeVowelsFromText "Thisisateststring"))
t_removeSpacesFromText_2 = TestCase(assertEqual "for removeSpacesFromText('This!is!a!test!string')" "This!is!a!test!string" (removeVowelsFromText "Thisisateststring"))



--removeSymbolsFromText

t_removeSymbolsFromText_1 = TestCase(assertEqual "For removeSymbolsFromText('This is a test string!'" "This is a test string" (removeSymbolsFromText "This is a test string!"))
t_removeSymbolsFromText_2 = TestCase(assertEqual "For removeSymbolsFromText('!This!is!a!test!string!'" "Thisisateststring" (removeSymbolsFromText "!This!is!a!test!string!"))
t_removeSymbolsFromText_3 = TestCase(assertEqual "For removeSymbolsFromText('!This!is!a!test!string!'" "Thisisateststring" (removeSymbolsFromText "!This!is!a!test!string!"))

--removeNonSymbolsFromText
t_removeNonSymbolsFromText_1 = TestCase(assertEqual "For removeNonSymbolsFromText('This is a test string!'" "!" (removeNonSymbolsFromText "This is a test string!"))
t_removeNonSymbolsFromText_2 = TestCase(assertEqual "For removeNonSymbolsFromText('!This!is!a!test!string!'" "!!!!!!" (removeNonSymbolsFromText "This is a test string!"))
