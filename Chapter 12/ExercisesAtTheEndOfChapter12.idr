--1. Write an updateGameState function with the following type: 
-- updateGameState : (GameState -> GameState) -> Command ()
-- find this function in the text below

--2. Implement the Functor, Applicative and Monad interfaces for Command. Answer see below
{-
mutual 
  Functor Command where
    map func x = do val <- x
                    pure (func val)

  Applicative Command where
    pure = Pure
    (<*>) f a = do f' <- f
                   a' <- a
                   pure (f' a')

  Monad Command where
    (>>=) = Bind
-}

import Data.Primitives.Views
import System

%default total

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel 
forever = More forever

random : Int -> Stream Int
random seed = let seed' = shiftR ((seed + 89438734) * 98438743) 2 in
                          seed :: (random seed')
                          
data Input = QuitCmd | Answer Int

record Score where
       constructor MkScore
       correct : Nat
       attempted : Nat

record GameState where
       constructor  MkGameState
       score : Score
       difficulty : Int

addCorrect : GameState -> GameState
addCorrect state = record {score -> correct $= (+1), score -> attempted $= (+1)} state

addAttempted : GameState -> GameState
addAttempted state = record {score -> attempted $= (+1)} state

initstate : GameState
initstate = MkGameState (MkScore 0 0) 12

Show GameState where
     show st = show (correct (score st)) ++ " / " ++ show (attempted (score st)) ++ ", diffic :  " ++ show (difficulty st) ++ "\n"

data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String
     
     GetState : Command GameState
     PutState : GameState -> Command GameState
     GetRundom : Command Int
     
     Pure : a -> Command a
     Bind : Command a -> (a -> Command b) -> Command b
     
data ConsoleIO : Type -> Type where
     Quit : ConsoleIO a 
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
     
namespace ConsoleIO
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

namespace CommandIO
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

processInput : (prompt : String) -> Command Input
processInput prompt = do PutStr prompt
                         res <- GetLine
                         case toLower res of
                              "quit" => Pure QuitCmd
                              answer => Pure (Answer (cast answer))

runCommand : Command a -> (rnds : Stream Int) -> (state : GameState) -> IO (a, Stream Int, GameState)
runCommand (PutStr x) rnds state = do putStr x
                                      pure ((), rnds, state)
runCommand GetLine rnds state = do x <- getLine
                                   pure (x, rnds, state)
runCommand GetState rnds state = pure (state, rnds, state)
runCommand (PutState x) rnds state = pure (x, rnds, x)
runCommand GetRundom (value :: xs) state = pure (value', xs, state) where
                                                    value' = makeInt value (difficulty state)
                                                    makeInt : Int -> Int -> Int
                                                    makeInt x y with (divides x y)
                                                      makeInt x 0 | DivByZero = 1
                                                      makeInt ((y * div) + rem) y | (DivBy prf) = (abs rem) + 1


                                           
runCommand (Pure x) rnds state = pure (x, rnds, state)
runCommand (Bind x f) rnds state = do (res, newRnds, newState) <- runCommand x rnds state
                                      runCommand (f res) newRnds newState

updateGameState : (GameState -> GameState) -> Command ()
updateGameState f = do st <- GetState
                       PutState (f st)
                       Pure ()

mutual 
  quiz : ConsoleIO GameState
  quiz = do st <- GetState
            PutStr ("Score so far: " ++ show st)
            val1 <- GetRundom
            val2 <- GetRundom
            ans <- processInput (show val1 ++ " * " ++ show val2 ++ " = ? " )
            case ans of QuitCmd => Quit
                        Answer answer => if val1 * val2 == answer then correct
                                                                         else wrong (val1 * val2)
            
  correct : ConsoleIO GameState
  correct = do PutStr "Correct! \n"
               updateGameState addCorrect
               quiz
  
  wrong : Int -> ConsoleIO GameState
  wrong x = do PutStr ("Wrong! Answer was " ++ show x ++ " \n"  )
               updateGameState addAttempted
               quiz

run : Fuel -> Stream Int -> GameState -> ConsoleIO GameState -> IO (Maybe GameState, Stream Int, GameState )
run Dry rnds state z = pure (Nothing, rnds, state)
run fuel rnds state Quit = pure (Just state, rnds, state)
run (More fuel) rnds state (Do z f) = do (res, newRnds, newState) <- runCommand z rnds state
                                         run fuel newRnds newState (f res)
partial
main : IO ()
main = do seed <- time
          (Just state, rnds, st) <- run forever (random (fromInteger seed)) initstate quiz 
                            | _ => putStr "run out of fuel"
          putStr ("Final score: " ++ show state)

