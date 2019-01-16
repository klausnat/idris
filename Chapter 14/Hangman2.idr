import Data.Vect
%default total

data Fuel = Dry | More (Lazy Fuel)

data GameState : Type where
     Running : (guesses : Nat) -> (letters : Nat) -> GameState
     NotRunning : GameState

letters : String -> List Char
letters str = nub (map toUpper (unpack str))

data GuessResult = Correct | Incorrect

data GameCmd : (ty : Type) -> (instate : GameState) -> (gameState_fn : ty -> GameState) -> Type where
     NewGame : (word : String) -> GameCmd () NotRunning (const $ Running 6 (length (letters word)))
     Won : GameCmd () (Running (S guesses) 0) (const NotRunning)
     Lost : GameCmd () (Running 0 (S letters)) (const NotRunning)
     Guess : (c : Char) -> GameCmd GuessResult (Running (S guesses) (S letters)) (\res => case res of 
                                                                                                  Correct => Running (S guesses) letters
                                                                                                  Incorrect => Running guesses (S letters))
     ReadGuess : GameCmd Char state (const state)
     (>>=) : GameCmd a state1 state2_fn ->
             ((res : a) -> GameCmd b (state2_fn res) state3_fn) ->
             GameCmd b state1 state3_fn
     
     Pure : (res : ty) -> GameCmd ty (state_fn res) state_fn
     Message : String -> GameCmd () state (const state)
     ShowState : GameCmd String state (const state)

namespace Loop          
  data GameLoop : (ty : Type) -> (instate : GameState) -> (gameState_fn : ty -> GameState) -> Type where
       (>>=) : GameCmd a state1 state2_fn -> ((res : a) -> Inf (GameLoop b (state2_fn res) state3_fn )) -> GameLoop b state1 state3_fn
       Exit : GameLoop () NotRunning (const NotRunning)
     
gameLoop : GameLoop () (Running (S guesses) (S letters)) (const NotRunning)
gameLoop {guesses} {letters} = do c <- ReadGuess
                                  ShowState
                                  g <- Guess c
                                  case g of Correct => case letters of Z => do Message "You win!"
                                                                               Won
                                                                               Exit
                                                                       (S ltrs) => do ShowState
                                                                                      gameLoop
                                            Incorrect => case guesses of Z => do Message "You Loose!"
                                                                                 Lost
                                                                                 Exit
                                                                         (S gss) => do ShowState
                                                                                       gameLoop
                                                                                
hangman : GameLoop () NotRunning (const NotRunning)
hangman = do NewGame "testing"  
             gameLoop

   
data Game : GameState -> Type where
     GameLost : (word : String) -> Game NotRunning
     GameWon : (word : String) -> Game NotRunning
     GameStart : Game NotRunning
     InProgress : (word : String) -> 
                  (guesses : Nat) -> 
                  (missing: Vect ltrs Char) -> Game (Running guesses ltrs)

Show (Game g) where
  show (GameLost word) = "Game Lost, word was " ++ word
  show (GameWon word) = "Game won, word was " ++ word
  show GameStart = "Starting game"
  show (InProgress word guesses missing) = "Game State: guesses - " ++ 
                                           show guesses ++ "\n" ++ 
                                           pack (map showLetter (unpack word)) where 
                                             showLetter : Char -> Char
                                             showLetter x = case isElem x missing of
                                                                                  Yes prf => '-'
                                                                                  No contra => x

data GameResult : (ty : Type) -> (ty -> GameState) -> Type where
     OK : (res : ty) -> Game (outstate_fn res) -> GameResult ty outstate_fn
     OutOfFuel : GameResult ty outstate_fn

ok : (res : ty) -> Game (outstate_fn res) -> IO (GameResult ty outstate_fn)
ok res st = pure (OK res st)


{-
runCmd : res -> Game (statefn res) -> GameCmd   -> IO (GameResult res outstate_fn)
                                             
run : Fuel -> 

-}

%default partial
forever : Fuel
forever = More forever
main : IO ()
