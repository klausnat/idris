import Data.Vect

%default total

data GameState : Type where
     NotRunning : GameState
     Running : (guesses : Nat) -> (letters : Nat) -> GameState

data GuessResult = Correct | Incorrect

data GameCmd : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
     NewGame : (word : String) -> GameCmd () NotRunning (const (Running 6 (length word))) 
     Won : GameCmd () (Running (S guesses) 0) (const NotRunning)
     Lost : GameCmd () (Running 0 (S letters)) (const NotRunning)
     Guess : (c : Char) -> GameCmd GuessResult 
                                   (Running (S guesses) (S letters)) 
                                   (\res => case res of Correct => (Running (S guesses) letters)
                                                        Incorrect => (Running guesses (S letters))) 
     Pure : (res : ty) -> GameCmd ty (state_fn res) state_fn
     (>>=) : GameCmd a state1 state2_fn ->  
             ((res : a) ->  GameCmd b (state2_fn res) state3_fn) ->
             GameCmd b state1 state3_fn

     ShowState : GameCmd String state (const state)
     Message : String -> GameCmd () state (const state)
     ReadGuess : GameCmd Char state (const state)  
     
letters : String -> List Char
letters str = nub (map toUpper (unpack str))

namespace Loop
  data GameLoop : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
       (>>=) : GameCmd a state1 state2_fn -> 
               ((res : a) -> Inf (GameLoop b (state2_fn res) state3_fn)) ->
               GameLoop b state1 state3_fn
       Exit : GameLoop () NotRunning (const NotRunning)
       
gameLoop : GameLoop () (Running (S guesses) (S letters)) (const NotRunning)
gameLoop {guesses} {letters} = do ShowState
                                  g <- ReadGuess
                                  ok <- Guess g
                                  case ok of Correct => case letters of 
                                                             Z => do Won
                                                                     ShowState
                                                                     Exit
                                                             S k => do Message "Correct!"
                                                                       gameLoop
                                                                        
                                             Incorrect => case guesses of
                                                               Z => do Lost
                                                                       ShowState
                                                                       Exit
                                                               S k => do Message "Wrong"
                                                                         gameLoop
                                  
hangman : GameLoop () NotRunning (const NotRunning)                                  
hangman = do NewGame "testing"
             gameLoop

data Game : GameState -> Type where
     GameStart : Game NotRunning
     GameWon : (word : String) -> Game NotRunning
     GameLost : (word : String) -> Game NotRunning
     InProgress : (word : String) -> (guesses : Nat) -> 
                  (missing : Vect letters Char) -> 
                  Game (Running guesses letters)

