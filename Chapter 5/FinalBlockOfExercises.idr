module Main

||| 1. Write a function readToBlank : IO (List String), that reads the input from the console until the user enters a blank line


readToBlank : IO (List String) -- returns actions that build a list of Strings
readToBlank = do x <- getLine
                 if x == "" then pure []
                 else do 
                         xs <- readToBlank
                         pure (x :: xs)


{- 2. Write a funciton readAndSave : IO (), 
 that reads input from the console until the user enters a blank line, 
 then reads a filename from the console and writes the input to that file -}
 
 
readAndSave : IO ()
readAndSave = do putStrLn "Enter text to write into a file (blank line to end)"
                 inp <- readToBlank
                 putStrLn "Enter filename"
                 filename <- getLine 
                 let resStr = unwords inp 
                 writeFile filename resStr
                 pure ()
