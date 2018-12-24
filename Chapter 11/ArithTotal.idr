import Data.Primitives.Views
import System

%default total

data InfIO : Type where 
     Do : IO a -> (a -> Inf InfIO) -> InfIO 

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO 
(>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> InfIO -> IO ()
run Dry y = putStrLn "Run out of fuel"
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)
                              
quiz : Stream Int -> (score : Nat) -> InfIO
quiz (val1 :: val2 :: vals) score = do putStrLn ("Current score is: " ++ cast score)
                                       putStr ((cast val1) ++ "*" ++ (cast val2) ++ " = ?")
                                       answer <- getLine
                                       if cast answer == val1 * val2 
                                          then do putStrLn "Correct!"
                                                  quiz vals (score + 1)
                                          else do putStrLn "Wrong!"
                                                  quiz vals score                              
randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'
                   
arithInputs : Int -> Stream Int                   
arithInputs seed = map bound (randoms seed) 
  where
    bound : Int -> Int
    bound x with (divides x 12)
      bound ((12 * div) + rem) | (DivBy prf) = abs rem + 1

partial
forever : Fuel
forever = More forever

partial 
main : IO ()          
main = do seed <- time
          run forever (quiz (arithInputs (fromInteger seed)) 0)

