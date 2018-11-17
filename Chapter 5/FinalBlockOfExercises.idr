module Main
import Data.Vect

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
                 

-- map : (a -> b) -> f a -> f b

{- 3. Write a function readVectFile that reads a contents of a file into a dependent pair 
containing a length and a Vect of that length -}

genVect : (h : File) -> (n ** Vect n String) -> IO (len ** Vect len String)     
genVect h (x ** pf) = do t <- fEOF h
                         case t of
                                  True  => pure (x ** pf)
                                  False => do Right line <- fGetLine h
                                                | Left err => pure (0 ** [])    
                                              genVect h (S x ** (line :: pf))

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename  =  do Right h <- openFile filename Read
                               | Left err => pure (0 ** [])
                             res <- genVect h (0 ** [])
                             closeFile h
                             pure res
                             

                                                                                   
