module Main

import System

||| Write a function that implements a simple "guess the number game"

readNumber : IO (Maybe Nat)
readNumber = do x <- getLine
                if all isDigit (unpack x)
                   then pure (Just (cast x))
                   else pure Nothing

guess : (target : Nat) -> (amountOfGuesses : Nat) -> IO ()
guess target n = do putStr "Used amount of guesses: "
                    putStrLn (show n)
                    putStr "Enter a guess: "
                    Just x <- readNumber | Nothing => do putStrLn ("Not a valid input")
                                                         guess target (S n)
                    if x == target then do putStrLn "Correct! You win!"
                                           pure ()
                    else if x < target then do putStrLn "Too low"
                                               guess target (S n)
                    else do putStrLn "Too high"
                            guess target (S n)

main : IO ()
main = do rand <- time
          let reminder = mod rand 100
          guess (cast reminder) Z

 
 
