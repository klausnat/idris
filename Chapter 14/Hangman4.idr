import Data.Vect
%default total
data Fuel = Dry | More (Lazy Fuel)

data GuessResult = Correct | Incorrect

data GameState : Type where
     Running : (guesses : Nat) -> (letters : Nat) -> GameState
     NotRunning : GameState

letters : String -> List Char
letters str = nub $ map toUpper (unpack str)

data GameCmd : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
     StartGame : (word : String) -> GameCmd () NotRunning (const (Running 6 (length (letters word))))
     Won : GameCmd () (Running (S guesses) 0) (const NotRunning)
     Lost : GameCmd () (Running 0 (S letters)) (const NotRunning)
     ReadGuess : GameCmd Char state (const state)
     Guess : (c : Char) -> 
             GameCmd GuessResult 
                     (Running (S guesses) (S letters)) 
                     (\res => case res of Correct => Running (S guesses) letters
                                          Incorrect => Running guesses (S letters))
      
     ShowState : GameCmd () state (const state)
     Message : (str : String) -> GameCmd () state (const state)
     (>>=) : GameCmd a state1 state2_fn ->
             ((res : a) -> GameCmd b (state2_fn res) state3_fn) ->
             GameCmd b state1 state3_fn
     Pure : (res : a) -> GameCmd a (state_fn res) state_fn

namespace Loop
  data GameLoop : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
       (>>=) : GameCmd a state1 state2_fn -> 
               ((res : a) -> Inf (GameLoop b (state2_fn res) state3_fn)) ->
               GameLoop b state1 state3_fn
       Exit : GameLoop () NotRunning (const NotRunning)

gameLoop : GameLoop () (Running (S guesses) (S letters)) (const NotRunning)

gameLoop {guesses} {letters} = do c <- ReadGuess
                                  g <- Guess c
                                  case g of Correct => do ShowState
                                                          Message "Correct!"
                                                          case letters of Z => do Won
                                                                                  Exit
                                                                          (S ltrs) => do gameLoop
                                    
                                            Incorrect => do ShowState
                                                            Message "Wrong!"
                                                            case guesses of Z => do Lost
                                                                                    Exit
                                                                            (S gss) => do gameLoop


hangman : GameLoop () NotRunning (const NotRunning)
hangman = do StartGame "testing"
             gameLoop

data Game : GameState -> Type where
     GameLost : (word : String) -> Game NotRunning
     GameWon : (word : String) -> Game NotRunning
     GameStart : (word : String) -> Game (Running _ (length (letters word)) )
     ShowCurState : (word : String) -> (guesses : Nat) -> (letters : Nat) -> Game (Running guesses letters)

Show (Game g) where
  show (GameLost word) = "You Loose, word was: " ++ word
  show (GameWon word) = "You win, word was: " ++ word
  show GameStart = "Starting game"
  show (ShowCurState word guesses ltrs) = "Guesses left: " ++ show guesses ++
                                                    (pack (map showLetter (unpack word))) where
                                               showLetter : Char -> Char
                                               showLetter c = case isElem c (fromList (letters word)) of
                                                                          Yes prf => '-'
                                                                          No contra => c 
                                             
data GameResult : (res : ty) -> (gamestate_fn res) -> Type where
     OK : (word : String) -> 
          (guesses : Nat) -> 
          (letters : Nat) -> 
          (missing : Vect letters Char ) -> GameResult res gamestate_fn
     OutOfFuel : GameResult res gamestate_fn

ok : (res : ty) -> (instate_fn res) -> IO (GameResult res outstate_fn)

%default partial
forever : Fuel
forever = More forever
main : IO ()
