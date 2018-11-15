module Main

import System

countdown : (n : Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown (S n) = do putStrLn (show (S n))
                     usleep 1000000
                     countdown n

readNumber : IO (Maybe Nat)
readNumber = do x <- getLine
                if all isDigit (unpack x)
                   then pure (Just (cast x))
                   else pure Nothing

countdowns : IO ()
countdowns = do putStr "Enter a number"
                Just startNum <- readNumber | Nothing => do putStrLn "Invalid input"
                                                            countdowns
                countdown startNum
                putStr "Another y/n? "
                yn <- getLine 
                if yn == "y" then countdowns else pure ()
                
