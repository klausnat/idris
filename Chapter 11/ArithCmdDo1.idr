import System
import Data.Primitives.Views
%default total

data Input = Answer Int | QuitCmd

data Command : Type -> Type where 
     PutStr : String -> Command ()
     GetLine : Command String
     Pure : ty -> Command ty
     Bind : Command a -> (a -> Command b) -> Command b

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (Pure x) = pure x
runCommand (Bind x f) = do res <- runCommand x 
                           runCommand (f res)

data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do
  
readInput : (prompt : String) -> Command Input
readInput prompt = do PutStr prompt
                      res <- GetLine
                      case toLower res of 
                              "quit" => Pure QuitCmd
                              answer  => Pure (Answer (cast answer))   

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

mutual 
  wrong : (score : Nat) -> (correctAnswer : Int) -> Stream Int -> ConsoleIO Nat
  wrong score correctAnswer xs = do PutStr ("Wrong, correct answer is: " ++ show correctAnswer ++ "\n")
                                    quiz score xs
  
  right : (score : Nat) -> Stream Int -> ConsoleIO Nat
  right score xs = do PutStr "Correct!"
                      quiz score xs

  quiz : (score : Nat) -> Stream Int -> ConsoleIO Nat
  quiz score (val1 :: val2 :: vals) = do PutStr ("Score so far: " ++ cast score ++ "\n")
                                         res <- readInput (show val1 ++ " * " ++ show val2 ++ " = ? ")
                                         case res of QuitCmd => Quit score
                                                     Answer answer => if answer /= val1 * val2 
                                                                          then wrong score (val1 * val2) vals 
                                                                          else right (score + 1) vals

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run Dry y = pure Nothing
run fuel (Quit y) = pure (Just y)
run (More fuel) (Do c f) = do res <- runCommand c
                              run fuel (f res)

randoms : Int -> Stream Int
randoms seed = let seed' = shiftR (seed * 737382 + 38383) 2 in
                                  seed :: randoms seed'

smallInputs : Int -> Stream Int
smallInputs seed = map bound (randoms seed) where
                             bound : Int -> Int
                             bound x with (divides x 12)
                               bound ((12 * div) + rem) | (DivBy prf) = rem + 1


partial
main : IO ()
main = do seed <- time
          Just res <- run forever (quiz 0 (smallInputs (fromInteger seed)))
               | Nothing => putStrLn "Run out of fuel"
          putStrLn ("Final score: " ++ show res)
