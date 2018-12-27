import System
import Data.Primitives.Views

%default total

data Input = Answer Int | QuitCmd

data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String
     Pure : a -> Command a
     Bind : Command a -> (a -> Command b) -> Command b
     
runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (Pure x) = pure x
runCommand (Bind x f) = do res <- runCommand x
                           runCommand (f res)

data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (Inf (a -> ConsoleIO b)) -> ConsoleIO b

namespace ConsoleDo
  (>>=) : Command a -> (Inf (a -> ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

processInput : (prompt : String) -> Command Input
processInput prompt = do PutStr prompt
                         input <- GetLine
                         case toLower input of
                              "quit" => Pure QuitCmd
                              answer => Pure (Answer (cast answer))

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel 
forever = More forever

mutual
  correct : (score : Nat) -> (vals: Stream Int) -> ConsoleIO Nat
  correct score vals = do PutStr "Correct!"
                          quiz score vals
  
  wrong : (score : Nat) -> (vals: Stream Int) -> Int -> ConsoleIO Nat
  wrong score vals x = do PutStr ("Wrong, correct answer was: " ++ show x ++ "\n")
                          quiz score vals

  quiz : (score : Nat) -> (vals : Stream Int) -> ConsoleIO Nat
  quiz score (val1 :: val2 :: vals) 
        = do PutStr ("Score so far: " ++ show score)
             answer <- processInput (show val1 ++ " * " ++ show val2 ++ " = ? " )
             case answer of
                  QuitCmd => Quit score
                  (Answer answer) => if answer == val1 * val2 then correct (score + 1) vals
                                                              else wrong score vals (val1 * val2)

randoms : Int -> Stream Int
randoms seed = let seed' = shiftR (seed * 2734783 + 383827) 2 in 
                                  seed :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs x = map bound (randoms x) where
                          bound : Int -> Int
                          bound x with (divides x 12)
                            bound ((12 * div) + rem) | (DivBy prf) = rem + 1



run : Fuel -> ConsoleIO a -> IO (Maybe a)
run Dry y = pure Nothing
run f (Quit y) = pure (Just y)
run (More fuel) (Do c f) = do res <- runCommand c
                              run fuel (f res)


partial
main : IO () 
main = do seed <- time 
          res <- run forever (quiz 0 (arithInputs (fromInteger seed)) )
          case res of Just x => putStrLn ("Final score: " ++ show x)
                      Nothing => putStrLn "Run out of fuel"
               
