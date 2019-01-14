import Data.Vect
%default total

data GuessResult = Correct | Incorrect

data GameState : Type where
     Running : (guesses : Nat) -> (letters : Nat) -> GameState 
     NotRunning : GameState

letters : String -> List Char
letters str = nub (map toUpper (unpack str))

data GameCmd : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
     NewGame : (word : String) -> GameCmd () NotRunning (const (Running 6 $ length (letters word)))
     Won : GameCmd () (Running (S guesses) 0) (const NotRunning)
     Lost : GameCmd () (Running 0 (S letters)) (const NotRunning)
     Guess : (c : Char) -> GameCmd GuessResult (Running (S guesses) (S letters)) 
                                                (\res => case res of 
                                                              Correct => (Running (S guesses) letters)
                                                              Incorrect => (Running (guesses) (S letters)))
     ReadGuess : GameCmd Char state (const state)
     
     Pure : (res : ty) -> GameCmd ty (state_fn res) state_fn
     (>>=) : GameCmd a state1 state2_fn  ->
             ((res : a) -> GameCmd b (state2_fn res) state3_fn) ->
             GameCmd b state1 state3_fn
     ShowState : GameCmd String state (const state)
     Message : String -> GameCmd () state (const state)

namespace Loop          
  data GameLoop : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
       (>>=) : GameCmd a state1 state2_fn ->
               ((res : a) -> Inf (GameLoop b (state2_fn res) state3_fn)) ->
               GameLoop b state1 state3_fn
       Exit : GameLoop () NotRunning (const NotRunning)

gameLoop : GameLoop () (Running (S guesses) (S letters)) (const NotRunning) 
gameLoop = ?gameLoop_rhs

-- hangman : GameLoop                    

data Game : GameState -> Type where
     GameWon : (word : String) -> Game NotRunning
     GameLost : (word : String) -> Game NotRunning
     InProgress : (word : String) -> (missing : Vect letters Char) -> (guesses : Nat) ->  Game (Running guesses letters)
     GameStart : Game NotRunning 
     
Show (Game g) where
  show (GameWon word) = "You win! Word was" ++ word
  show (GameLost word) = "You loose! Word was" ++ word
  show (InProgress word vectMissing guesses) = "Guesses left: " ++ show guesses 
                                                        ++ pack (map showLetter (unpack word)) ++ "\n" where
                                                           showLetter : Char -> Char
                                                           showLetter x = case isElem x vectMissing of
                                                                               Yes prf => '-'
                                                                               No contra => x
  show GameStart = "Starting the game \n"

data Fuel = Dry | More (Lazy Fuel)

partial 
forever : Fuel 
forever = More forever

main : IO ()
