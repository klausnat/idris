-- попытка номер 1
import System
import Data.Primitives.Views

%default total

data Fuel = Dry | More (Lazy Fuel)

data Input = QuitCmd | Answer Int

record Score where
   constructor MkScore
   answered : Nat
   attempted : Nat

  
record GameState where
   constructor MkGameState
   score : Score
   difficulty : Int

%name GameState state, st

updateDifficulty : (newDif : Int) -> GameState -> GameState
updateDifficulty newDif state = record {difficulty = newDif} state

updateRightAnswers : GameState -> GameState
updateRightAnswers state = record {score -> answered $= (+1),
                                   score -> attempted $= (+1)} state

updateWrongAnswers : GameState -> GameState
updateWrongAnswers state = record {score -> attempted $= (+1)} state

initialState : GameState
initialState = MkGameState (MkScore 0 0) 12

Show GameState where
  show st = "Current score: " ++ 
            show (answered (score st)) ++ " / " ++ 
            show (attempted (score st)) ++ 
            "Diffic: " ++ show (difficulty st)

data Command : Type -> Type where     
     PutStr : String -> Command ()
     GetLine : Command String
     
     GetRandom : Command Int
     PutGameState : GameState -> Command ()
     GetGameState : Command GameState
     
     Pure : a -> Command a
     Bind : Command a -> (a -> Command b) -> Command b
     
data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace ConsoleIO
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do
  
namespace CommandIO     
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind
                              
partial
forever : Fuel
forever = More forever

processInput : (prompt : String) -> Command Input
processInput prompt = do PutStr prompt
                         res <- GetLine
                         case toLower res of
                              "exit" => Pure QuitCmd
                              answer => Pure (Answer (cast answer))

mutual 
  wrong : Int -> ConsoleIO GameState
  
  right : ConsoleIO GameState
  
  quiz : ConsoleIO GameState
  quiz = do val1 <- GetRandom
            val2 <- GetRandom
            input <- processInput (show val1 ++ show val2 ++ " = ? ")
            case input of
                 QuitCmd => ?ss
                 Answer answer => if val1 * val2 == answer then right
                                                            else wrong (val1 * val2)
  

randoms : Int -> Stream Int
randoms seed = let seed' = shiftR (seed * 983472 + 290393) 2 in
               seed :: (randoms seed')

runCommand : Command a -> Stream Int -> (a, Stream Int, GameState)

run : Fuel -> Stream Int -> GameState -> ConsoleIO GameState -> IO (Maybe a, Stream Int, GameState)

partial
main : IO ()
{- main = do seed <- time
          (Just val, _, state) <- run forever (randoms (toInteger seed)) initialState quiz                                                      | _ => putStrLn "Run out of fuel"
          putStrLn (show state)
                                   
-}
