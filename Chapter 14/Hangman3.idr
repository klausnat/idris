import Data.Vect

%default total

data Fuel = Dry | More (Lazy Fuel)

data GuessResult = Correct | Incorrect

data GameState : Type where
     Running : (guesses : Nat) -> (letters : Nat) -> GameState 
     NotRunning : GameState

letters : String -> List Char
letters str = nub (map toUpper (unpack str))

data GameCmd : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
     StartGame : (word : String) -> GameCmd () NotRunning (const $ Running 6 $ length (letters word))
     Lost : GameCmd () (Running 0 (S letters)) (const NotRunning)
     Won : GameCmd () (Running (S guesses) 0) (const NotRunning)
     Message : String -> GameCmd () state (const state)
     GetState : GameCmd GameState state (const state)
     (>>=) : (res : a) -> GameCmd a (state2_fn res) state2_fn
     Pure : GameCmd a state1 state2_fn -> 
            ((res : a) -> (GameCmd b (state2_fn res) state3_fn)) -> 
            GameCmd b state1 state3_fn
     Guess : Char -> 
             GameCmd GuessResult
             (Running (S guesses) (S letters)) 
             (\res => case res of
                           Correct => (Running (S guesses) letters)
                           Incorrect => (Running (guesses) (S letters))
             ) 
     GetGuess : GameCmd Char state (const state)

namespace Loop
  data GameLoop : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
       (>>=) : GameCmd a state1 state2_fn -> 
               ((res : a) -> Inf (GameLoop b (state2_fn res) state3_fn)) ->
               GameLoop b state1 state3_fn
       Exit : GameLoop () NotRunning (const NotRunning)

gameLoop : GameLoop () (Running (S guesses) (S letters)) (const NotRunning)
gameLoop {guesses} {letters} = do c <- GetGuess
                                  g <- Guess c
                                  case g of Correct => 
                                              case letters of Z => do Message "Correct!"
                                                                      Won
                                                                      Exit
                                                              (S ltrs) => do Message "Correct!"
                                                                             gameLoop
                                            Incorrect => 
                                              case guesses of Z => do Message "Wrong!"
                                                                      Lost
                                                                      Exit
                                                              (S ltrs) => do Message "Wrong"
                                                                             gameLoop
                                                                         

hangman : GameLoop () NotRunning (const NotRunning)
hangman = do StartGame "testing"
             gameLoop

data Game : GameState -> Type where
     GameLost : (word : String) -> Game NotRunning
     GameWon : (word : String) -> Game NotRunning
     GameStart : Game NotRunning  
     InProcess : (word : String) -> (guesses : Nat) -> (missing : Vect ltrs Char) -> Game (Running guesses ltrs)

Show (Game g) where
  show (GameLost word) = "Game Lost, word was " ++ word
  show (GameWon word) = "Game Won, word was " ++ word
  show GameStart = "Let's start the game"
  show (InProcess word guesses missing) = "Guesses left: " ++ show guesses ++ "\n" ++
                                          (pack (map showLetter (unpack word))) 
                                                where
                                                         showLetter : Char -> Char
                                                         showLetter c = case isElem c missing of
                                                                             Yes prf => '-'
                                                                             No contra => c   
                                     

data GameResult : (ty : Type) -> (ty -> GameState) -> Type where
     OK : (res : ty) -> Game (outstate_fn res) -> GameResult ty outstate_fn
     OutOfFuel : GameResult ty outstate_fn

ok : (res : ty) -> Game (outstate_fn res) -> IO (GameResult ty outstate_fn)
ok res st = pure (OK res st)

runCmd : Fuel -> Game instate -> GameCmd ty instate outstate_fn -> IO (GameResult ty outstate_fn)
runCmd Dry y z = pure OutOfFuel
runCmd (More x) y (StartGame word) = ?runCmd_rhs_1
runCmd (More x) y Lost = ?runCmd_rhs_3
runCmd (More x) (InProcess word _ missing) Won = ok () (GameWon word)
runCmd (More x) y (Message z) = ?runCmd_rhs_5
runCmd (More x) y GetState = ?runCmd_rhs_6
runCmd (More x) y ((>>=) res) = ?runCmd_rhs_7
runCmd (More x) y (Pure z f) = ?runCmd_rhs_8
runCmd (More x) y (Guess z) = ?runCmd_rhs_9
runCmd (More x) y GetGuess = ?runCmd_rhs_10

%default partial

forever : Fuel
forever = More forever
main : IO ()
