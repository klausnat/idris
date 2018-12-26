import System
import Data.Primitives.Views

%default total

data Input = Answer Int | QuitCmd

data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String
     Pure : ty -> Command ty
     Bind : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever     
               
namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do
  
runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (Pure x) = pure x
runCommand (Bind c f) = do res <- runCommand c
                           runCommand (f res)

readInput : (prompt : String) -> Command Input
readInput prompt = do PutStr prompt
                      answer <- GetLine
                      if toLower answer == "quit"
                         then Pure QuitCmd
                         else Pure (Answer (cast answer))

mutual
  right : (score : Nat) -> (amOfQ : Nat) -> Stream Int -> ConsoleIO (Nat,Nat)
  right score amOfQ xs = do PutStr "Correct!"
                            quiz xs score amOfQ

  wrong : (correctA : Int) -> (score : Nat) -> (amOfQ : Nat) -> Stream Int -> ConsoleIO (Nat,Nat) 
  wrong correctA score amOfQ xs = do PutStr ("Wrong answer, correct one is: " ++ show correctA ++ "\n")
                                     quiz xs score amOfQ
  
  quiz : Stream Int -> (score : Nat) -> (amOfQ : Nat) -> ConsoleIO (Nat,Nat)
  quiz (num1 :: num2 :: nums) score amOfQ
     = do PutStr ("Score so far: answers: " ++ show score ++ " / questions: " ++ show amOfQ ++ "\n")
          reply <- readInput (show num1 ++ " * " ++ show num2 ++ " = ? " )
          case reply of 
            QuitCmd => Quit (score, amOfQ)
            Answer answer => if answer == num1 * num2 then right (score + 1) (amOfQ + 1) nums
                                                      else wrong (num1 * num2) score (amOfQ + 1) nums

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run fuel (Quit x) = do pure (Just x)
run Dry p = pure Nothing
run (More fuel) (Do c f) = do res <- runCommand c
                              run fuel (f res) 

randoms : Int -> Stream Int
randoms seed = let seed' = shiftR (seed * 93485 + 203423) 2 in
                   seed :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs x = map bound (randoms x) where
                          bound : Int -> Int
                          bound x with (divides x 12)
                            bound ((12 * div) + rem) | (DivBy prf) = rem + 1


partial
main : IO ()
main = do seed <- time
          Just (score, questions) <- run forever (quiz (arithInputs (fromInteger seed)) 0 0)
              | Nothing => putStrLn "Run out of fuel"
          putStrLn ("Final score: " ++ "answers: " ++ show score ++ "/ questions: " ++ show questions ) 
