module Main where


import Analyzer
import System.IO
import Control.Exception
--
-- data Text = RawText String | TransformedText String String String


----- File handling -----

--getTextfileContents :: String -> String
--getTextFileContents [] = putStr "No input path"
--getTextfileContents input_path = do
--handle <- openFile input_path ReadMode
  --contents <- hGetContents handle
  --hClose handle
  --contents

  getTextfileContents :: String -> String
  getTextfileContents input_path = do
  handle <- openFile input_path ReadMode
    contents <- hGetContents handle
    --hClose handle
    --contents


writeTextFile :: String -> String -> IO()
writeTextFile input_path input_contents = do
  writeFile input_path input_contents


--validateTextFile :: String -> IO Bool
--validateTextFile input_path = do
--  handle (\(e :: IOException) -> print e >> return Nothing)$ do
--  putStrLn "Validating..."
--  h <- openFile "/some/path" ReadMode
  --return (Just (0 == length s))
    -- return True


-- Processes
-- Can only get the IO String as a String in the function it is read
performSimpleAnalysis :: String -> String -> IO()
performSimpleAnalysis input_path output_path = do
  putStrLn "Processing..."
  s <- readFile input_path
  writeFile output_path (analyzeStringSimpleOutput s)
  putStrLn "Text processed successfully"



----- User interaction -----
-- promptForIntOption :: Int -> String -> Int
-- promptForIntOption maxOption message = do
--    putStr "input:" --instruction text
--    input <- getLine -- get input
--    x <- (read input :: Int) --try to validate it as an int
--    if x > maxOption then return 0 --break output
--    else if x < 1 then 0
--    else 1 --correct output


-- Where options can be selected
-- Transformations can be applied to strings, but this one does it to an input text files
-- Allows greater interaction
-- Pretty hardcoded, will come up with more streamlined option if time is availible

-- User input stuff which selects which selects the functions
displayOptionsText ::  String -> String -> IO()
displayOptionsText title_s options_s = do
  putStrLn (title_s)
  putStrLn (options_s)


promptForDouble :: String -> IO Double
promptForDouble message = do
     putStr message
     d <- readLn
     return d

main :: IO ()
main = do
  -- displayOptionsText "Please Select an option\n\n" "1:Simple Analysis\n2:Detailed Analysis\n3:Back\n\n" 3
  print "filler"
