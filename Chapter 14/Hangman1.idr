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
gameLoop {guesses} {letters} = do ShowState
                                  c <- ReadGuess
                                  r <- Guess c
                                  case r of Correct => case letters of 
                                                            (S ltrs) => do Message "Correct"
                                                                           gameLoop
                                                            Z => do Won
                                                                    ShowState
                                                                    Exit
                                            Incorrect => case guesses of 
                                                              (S gss) => do Message "Wrong"
                                                                            gameLoop
                                                              Z => do Lost
                                                                      ShowState
                                                                      Exit
                                                                                           
              

hangman : GameLoop () NotRunning (const NotRunning)
hangman = do NewGame "testing"
             gameLoop
 
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

data GameResult : (ty : Type) -> (ty -> GameState) -> Type where
     OK : (res : ty) -> Game (outstate_fn res) -> GameResult ty (outstate_fn)
     OutOfFuel : GameResult ty outstate_fn
  

ok : (res : ty) -> Game (outstate_fn res) -> IO (GameResult ty outstate_fn)
ok res st = pure (OK res st)

runCmd : Fuel -> Game instate -> GameCmd ty instate outstate_fn -> IO (GameResult ty outstate_fn)
runCmd Dry _ _ = pure OutOfFuel
runCmd (More fuel) instate (NewGame word) = ok () (InProgress (toUpper word) (fromList (letters word)) _ )
runCmd (More fuel) (InProgress word missing _) Won = ok () (GameWon word)
runCmd (More fuel) (InProgress word _ _) Lost = ok () (GameLost word)
runCmd (More fuel) (InProgress word missing _) (Guess c) 
  = case isElem c missing of
                Yes prf => ok Correct (InProgress word (dropElem missing prf) _)
                No contra => ok Incorrect (InProgress word missing _)
runCmd (More fuel) instate ReadGuess = do putStr "Guess: "
                                          g <- getLine
                                          case unpack g of
                                               [x] => case isAlpha x of
                                                           True => ok (toUpper x) instate
                                                           False => do putStrLn "Invalid input"
                                                                       runCmd fuel instate ReadGuess
                                               _ => do putStrLn "Invalid input"
                                                       runCmd fuel instate ReadGuess
                                          
runCmd (More fuel) instate (Pure res) = ok res instate
runCmd (More fuel) instate (cmd >>= next) = do OK cmdRes newSt <- runCmd fuel instate cmd
                                                    | OutOfFuel => pure OutOfFuel
                                               runCmd fuel newSt (next cmdRes)
runCmd (More fuel) instate ShowState = do putStrLn (show instate)
                                          ok "Current state: " instate
runCmd (More fuel) instate (Message x) = do putStrLn x 
                                            ok () instate

run : Fuel -> Game instate -> GameLoop ty instate outstate_fn -> IO (GameResult ty outstate_fn)
run Dry y z = pure OutOfFuel
run (More fuel) instate (z >>= f) = do OK res newSt <- runCmd fuel instate z
                                           | _ => pure OutOfFuel
                                       run fuel newSt (f res)

run fuel y Exit = ok () y

%default partial 
forever : Fuel 
forever = More forever

main : IO ()
main = do run forever GameStart hangman
          pure ()
