import Average

showAverage : String -> String 
showAverage str = ?hole 
-- "The average word length is: " ++ 
--                   show (average str) ++ "\n"                  
                                                      
main : IO ()
main = repl "Enter a string: "
            showAverage                  
