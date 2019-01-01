-- CHAPTER 12 QUIZ with STATE and RECORDS
import System
import Data.Primitives.Views

%default total

record Score where
       constructor MkScore
       correct : Nat
       attempted : Nat
       
record GameState where
       constructor MkGameState
       score : Score
       difficulty: Int

data Input = QuitCmd | Answer Int

data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String
     
     GetRandom : Command Int
     GetGameState : Command GameState
     PutGameState : GameState -> Command ()
     
     Pure : a -> Command a
     Bind : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
     
namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleDo     
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever
       
initState : GameState
initState = MkGameState (MkScore 0 0) 12       

setDifficulty : Int -> GameState -> GameState
setDifficulty newDiff = record {difficulty = newDiff}

addWrong : GameState -> GameState
addWrong state = record {score -> attempted $= (+ 1)} state

addCorrect : GameState -> GameState
addCorrect state = record {score -> correct $=  (+ 1),
                           score -> attempted $= (+ 1)} state

Show GameState where
   show st = show (correct (score st)) ++ " / " ++
             show (attempted (score st)) ++ 
             " Difficulty " ++ show (difficulty st)

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

runCommand : Stream Int -> GameState -> Command a -> IO (a, Stream Int, GameState)
runCommand rnds state (PutStr y) = do putStr y 
                                      pure ((), rnds, state)
runCommand rnds state GetLine = do str <- getLine
                                   pure (str, rnds, state)
runCommand (value :: rnds) state GetRandom = pure (getRandom value (difficulty state), rnds, state)
              where
                getRandom : Int -> Int -> Int
                getRandom val max with (divides val max)
                  getRandom val 0 | DivByZero = 1
                  getRandom ((max * div) + rem) max | (DivBy prf) = abs rem + 1

                
runCommand rnds state GetGameState = pure (state, rnds, state)
runCommand rnds state (PutGameState newState) = pure ((), rnds, newState)
runCommand rnds state (Pure val) = pure (val, rnds, state)
runCommand rnds state (Bind c f) = do (res, rnds', state') <- runCommand rnds state c
                                      runCommand rnds' state' (f res)
                                      
run : Fuel -> Stream Int -> GameState -> ConsoleIO a -> IO (Maybe a, Stream Int, GameState)
run Dry rnds state p = pure (Nothing, rnds, state)
run fuel rnds state (Quit z) = pure (Just z, rnds, state)
run (More fuel) rnds state (Do c f) = do (a, rnds', state') <- runCommand rnds state c
                                         run fuel rnds' state' (f a)

mutual 
  correct : ConsoleIO GameState
  correct = do PutStr "Correct!"
               st <- GetGameState
               PutGameState (addCorrect st)
               quiz
  
  wrong : Int -> ConsoleIO GameState
  wrong corAns = do PutStr ("Wrong, answer is: " ++ show corAns ++ "\n")
                    st <- GetGameState
                    PutGameState (addWrong st)
                    quiz
  
  readInput : (prompt : String) -> Command Input
  readInput prompt = do PutStr prompt
                        inp <- GetLine
                        case toLower inp of 
                             "exit" => Pure QuitCmd
                             answ => Pure (Answer (cast answ))
  
  quiz : ConsoleIO GameState
  quiz = do val1 <- GetRandom
            val2 <- GetRandom
            st <- GetGameState
            PutStr (show st ++ "\n")
            input <- readInput (show val1 ++ " * " ++ show val2 ++ " = ? ")
            case input of 
                 QuitCmd => Quit st
                 Answer answer => if answer == val1 * val2 then correct
                                                           else wrong (val1 * val2)

randoms : Int -> Stream Int
randoms seed = let res = shiftR ((seed + 230423) * 2223243) 2 in
                   seed :: (randoms res)

partial 
main : IO ()
main = do seed <- time
          (Just score, _, state ) <- run forever (randoms (fromInteger seed)) initState quiz
                                         | _ => putStrLn "Ran out of fuel"
          putStrLn ("Final score" ++ show state)        

