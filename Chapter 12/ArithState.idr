-- CHAPTER 12 QUIZ with STATE and RECORDS

GameState : Type

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

forever : Fuel
forever = More forever

record Score where
       constructor MkScore
       correct : Nat
       attempted : Nat
       
record GameState where
       constructor MkGameState
       score : Score
       difficulty: Int
       
initState : GameState
initState = MkGameState (MkScore 0 0) 12       

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

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine

runCommand (GetRandom) = ?s
runCommand (GetGameState) = ?i
runCommand (PutGameState x) = ?y

runCommand (Pure x) = pure x
runCommand (Bind x f) = do res <- runCommand x
                           runCommand (f res)
                           
run : Fuel -> ConsoleIO a -> IO (Maybe a)                           
run Dry y = pure Nothing
run fuel (Quit y) = pure (Just y)
run (More x) (Do z f) = do res <- runCommand z
                           run x (f res)





