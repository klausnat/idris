module Main 
import Data.Vect

anyVect : (n : Nat ** Vect n String)
anyVect = (4 ** ["Natasha", "Andrey", "Vova", "Sasha"])

readVect : IO (len ** Vect len String)
readVect = do x <- getLine 
              if x == "" then pure (_ ** [])
              else do (_ ** xs) <- readVect 
                      pure (_ ** x :: xs)  
-- exactLength

zipInputs : IO ()
zipInputs = do putStrLn "Enter first vector (blank line to end):"
               (len1 ** vec1) <- readVect
               putStrLn "Enter second vector (blank line to end):"
               (len2 ** vec2) <- readVect
               case exactLength len1 vec2 of
                    Just (vec2') => printLn (zip vec1 vec2')
                    Nothing => putStrLn "Vectors are diff lengths"
               
