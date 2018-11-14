module Main

||| Exercise 1: Using do notation write a printLonger string that reads two strings and then displays the length of the longer one.

printLonger : IO ()
printLonger = do 
               putStr "String #1: "
               x <- getLine 
               putStrLn ""
               putStr "String #2: "
               y <- getLine
               if Strings.length x > Strings.length y then putStrLn (show (Strings.length x)) else putStrLn (show (Strings.length y))

||| Exercise 2: Write the same program using >>= instead of do notation

printLonger' : IO ()
printLonger' = putStr "Input string 1:" >>= \_ =>
              getLine >>= \st1 =>
              putStr "Input string 2:" >>= \_ =>
              getLine >>= \st2 =>
              let lenToShow = if length st1 > length st2 then length st1 else length st2 in
              putStrLn (show lenToShow)

main : IO ()
main = printLonger'
