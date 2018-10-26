module Main

average : (str : String) -> Double
average str = let tw = totalWords str
                  tl = totalLetters str in
                  cast tl / cast tw 
         where
           totalWords : String -> Nat
           totalWords str = length (words str)

           totalLetters : String -> Nat 
           totalLetters str = sum (map length (words str))

showAverage : String -> String 
showAverage str = "The average word length is: " ++ 
                  show (average str) ++ "\n"
