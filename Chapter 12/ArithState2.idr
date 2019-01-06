import Data.Primitives.Views
import System

data Fuel = Dry | More (Lazy Fuel)

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
     show st = show (correct (score st)) ++ " / " ++ show (attempted (score st)) ++ ", diffic :  " ++ show (difficulty st)

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

runCommand : Command a -> (rnds : Stream Int) -> (state : GameState) -> IO (a, rnds, state)
runCommand (PutStr x) rnds state = (putStr x, rnds, state)
runCommand GetLine rnds state = ?runCommand_rhs_2
runCommand GetState rnds state = ?runCommand_rhs_3
runCommand (PutState x) rnds state = ?runCommand_rhs_4
runCommand GetRundom rnds state = ?runCommand_rhs_5
runCommand (Pure x) rnds state = ?runCommand_rhs_6
runCommand (Bind x f) rnds state = ?runCommand_rhs_7

mutual 
  quiz : ConsoleIO GameState
  quiz = do st <- GetState
            PutStr ("Score so far: " ++ show st)
            val1 <- GetRundom
            val2 <- GetRundom
            ans <- processInput (show val1 ++ " * " ++ show val2 ++ " = ? " )
            case ans of QuitCmd => Quit
                        Answer answer => if val1 * val2 == answer then correct
                                                                         else wrong
            
  correct : ConsoleIO GameState
  correct = do st <- GetState
               PutState (addCorrect st)
               quiz
  
  wrong : ConsoleIO GameState
  wrong = do st <- GetState
             PutState (addAttempted st)
             quiz

