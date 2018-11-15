module Main

||| Write a function that implements a simple "guess the number game"

readNumber : IO (Maybe Nat)
readNumber = do x <- getLine
                if all isDigit (unpack x)
                   then pure (Just (cast x))
                   else pure Nothing

guess : (target : Nat) -> IO ()
guess target = do putStr "Enter a guess: "
                  Just x <- readNumber | Nothing => do putStrLn ("Not a valid input")
                                                       guess target
                  if x == target then do putStrLn "You win!"
                                         pure ()
                                 else do putStrLn ("Nope!")
                                         guess target

