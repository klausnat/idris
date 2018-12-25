import Data.Primitives.Views
import System

%default total

data Command : Type -> Type where
     PutStrLn : String -> Command ()
     GetLine : Command String

data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
     
(>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
(>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

runCommand : Command a -> IO a
runCommand (PutStrLn x) = putStrLn x
runCommand GetLine = getLine

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run Dry y = pure Nothing
run fuel (Quit val) = do pure (Just val)
run (More fuel) (Do c f) = do res <- runCommand c
                              run fuel (f res)

partial
forever : Fuel
forever = More forever

mutual 

  correct : (acc : Nat) -> (Stream Int) -> ConsoleIO Nat
  correct acc xs = do PutStrLn "Right!"
                      quiz (acc + 1) xs

  wrong : (correctAnswer : Int) -> (acc : Nat) -> (Stream Int) -> ConsoleIO Nat
  wrong correctAnswer acc vals = do PutStrLn ("Wrong, correct answer is: " ++ show correctAnswer)
                                    quiz acc vals

  quiz : (acc : Nat) -> Stream Int -> ConsoleIO Nat
  quiz acc (val1 :: val2 :: vals) = do PutStrLn ("score is: " ++ show acc)
                                       PutStrLn (show val1 ++ " * " ++ show val2 ++ " = ? ")
                                       answer <- GetLine 
                                       if toLower answer == "quit" then Quit acc else
                                         if cast answer /= val1 * val2 
                                             then wrong (val1 * val2) acc vals
                                             else correct acc vals
                                       


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
          Just score <- run forever (quiz 0 (arithInputs (fromInteger seed)))
              | Nothing => putStrLn "Run out of fuel"
          putStrLn ("Final score: " ++ show score) 

