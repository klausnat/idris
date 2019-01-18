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
      
     ShowState : GameCmd String state (const state)
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
     GameStart : Game NotRunning
     GameLost : (word : String) -> Game NotRunning
     GameWon : (word : String) -> Game NotRunning
     InProcess : (word : String) -> 
                 (guesses : Nat) -> 
                 (missing : Vect ltrs Char) -> Game $ Running guesses ltrs


Show (Game g) where
  show (GameLost word) = "You Loose, word was: " ++ word
  show (GameWon word) = "You win, word was: " ++ word
  show GameStart = "Starting the game"
  show (InProcess word guesses missing) = "Guesses left: " ++ show guesses ++
                                                    (pack (map showLetter (unpack word))) where
                                               showLetter : Char -> Char
                                               showLetter c = case isElem c missing of
                                                                          Yes prf => '-'
                                                                          No contra => c 
data GameResult : (ty : Type) -> (ty -> GameState) -> Type where
     OK : (res : ty) -> Game (outstate_fn res) -> GameResult ty outstate_fn
     OutOfFuel : GameResult ty outstate_fn

ok : (res : ty) -> Game (outstate_fn res) -> IO (GameResult ty outstate_fn)
ok res x = pure (OK res x)

runCmd : Fuel -> Game instate -> GameCmd ty instate outstate_fn -> IO (GameResult ty outstate_fn)
runCmd Dry _ _ = pure OutOfFuel
runCmd (More fuel) instate (StartGame word) = ok () (InProcess (toUpper word) _ (fromList (letters word)))
runCmd (More fuel) (InProcess word _ missing) Won = ok () (GameWon word)
runCmd (More fuel) (InProcess word _ missing) Lost = ok () (GameLost word)
runCmd (More fuel) instate ReadGuess = do putStr "Guess: "
                                          g <- getLine
                                          case unpack g of
                                            [x] => case isAlpha x of
                                                        True => ok (toUpper x) instate
                                                        False => do putStrLn "Invalid input"
                                                                    runCmd fuel instate ReadGuess
                                            _ => do putStrLn "Invalid input"
                                                    runCmd fuel instate ReadGuess
runCmd (More fuel) (InProcess word _ missing) (Guess c) 
       = do case isElem c missing of
                 Yes prf => ok Correct (InProcess word _ (dropElem missing prf))
                 No contra => ok Incorrect (InProcess word _ missing)
runCmd (More fuel) instate ShowState = do printLn instate 
                                          ok (show instate) instate
runCmd (More fuel) instate (Message str) = do putStrLn str
                                              ok () instate
runCmd (More fuel) instate (cmd >>= next) = do OK res outSt <- runCmd fuel instate cmd
                                                            | _ => pure OutOfFuel
                                               runCmd fuel outSt (next res)
runCmd (More fuel) instate (Pure res) = ok res instate

run : Fuel -> Game instate -> GameLoop ty instate outstate_fn -> IO (GameResult ty outstate_fn)
run Dry y z = pure OutOfFuel 
run (More fuel) state (cmd >>= next) = do OK cmdRes newSt <- runCmd fuel state cmd
                                                | _ => pure OutOfFuel
                                          run fuel newSt (next cmdRes)                   
run (More x) y Exit = ok () y

%default partial
forever : Fuel
forever = More forever
main : IO ()
main = do run forever GameStart hangman
          pure ()
