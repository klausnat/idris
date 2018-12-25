import Data.Primitives.Views
import System

%default total

data InfIO : Type where
     Do : IO a -> (a -> Inf InfIO) -> InfIO 

(>>=) :  IO a -> (a -> Inf InfIO) -> InfIO 
(>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> InfIO -> IO ()
run Dry y = putStrLn "Run out of fuel"
run (More fuel) (Do x f) = do res <- x
                              run fuel (f res)

partial
forever : Fuel
forever = More forever

quiz : (acc : Nat) -> Stream Int -> InfIO
quiz acc (val1 :: val2 :: vals) = do putStrLn ("score is: " ++ show acc)
                                     putStrLn (show val1 ++ " * " ++ show val2 ++ " = ? ")
                                     answer <- getLine 
                                     if cast answer /= val1 * val2 then do putStrLn "Wrong!" 
                                                                           quiz acc vals
                                                                   else do putStrLn "Right!"
                                                                           quiz (acc + 1) vals

randoms : Int -> Stream Int
randoms x = let x' = (shiftR (x * 84747387 + 98383736) 2) in
                     x :: randoms x'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
                         where 
                           bound : Int -> Int
                           bound x with (divides x 12)
                             bound ((12 * div) + rem) | (DivBy prf) = abs rem + 1



partial 
main : IO ()
main = do seed <- time
          run forever (quiz 0 (arithInputs (fromInteger seed)))
