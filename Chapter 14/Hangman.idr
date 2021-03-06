import Data.Vect

%default total

data GameState : Type where
     NotRunning : GameState
     Running : (guesses : Nat) -> (letters : Nat) -> GameState

data GuessResult = Correct | Incorrect

letters : String -> List Char
letters str = nub (map toUpper (unpack str))

data GameCmd : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
     NewGame : (word : String) -> GameCmd () NotRunning (const (Running 6 (length (letters word)))) 
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
                  (missing : Vect ltrs Char) -> 
                  Game (Running guesses ltrs)

Show (Game g) where
  show GameStart = "Starting"
  show (GameWon word) = "Game won, word was: " ++ word
  show (GameLost word) = "Game lost, word was: " ++ word
  show (InProgress word guesses missing) = (pack (map showLetter (unpack word))) 
                                           ++ "\n" ++ show guesses ++ " guesses left"  where
                                             showLetter : Char -> Char 
                                             showLetter x = if elem x missing then '-' else x

data Fuel = Dry | More (Lazy Fuel)

data GameResult : (ty : Type) -> (ty -> GameState) -> Type where
     OK : (res : ty) -> Game (outstate_fn res) -> GameResult ty outstate_fn
     OutOfFuel : GameResult ty outstate_fn

ok : (res : ty) -> Game (outstate_fn res) -> IO (GameResult ty outstate_fn)
ok res st = pure (OK res st)

runCmd : Fuel -> Game instate -> GameCmd ty instate outstate_fn -> IO (GameResult ty outstate_fn) 
runCmd fuel state (NewGame word) = ok () (InProgress (toUpper word) _ (fromList (letters word))) 
runCmd fuel (InProgress word _ missing) Won = ok () (GameWon word)
runCmd fuel (InProgress word _ missing) Lost = ok () (GameLost word)
runCmd fuel (InProgress word _ missing) (Guess c) 
  = case isElem c missing of
      Yes prf => ok Correct (InProgress word _ (dropElem missing prf))
      No contra => ok Incorrect (InProgress word _ missing)
runCmd fuel state (Pure res) = ok res state
runCmd fuel st (cmd >>= next) = do OK cmdRes newSt <- runCmd fuel st cmd
                                         | _ => pure OutOfFuel
                                   runCmd fuel newSt (next cmdRes)
runCmd fuel state ShowState = do printLn state
                                 ok "state printed" state
runCmd fuel state (Message x) = do putStrLn x
                                   ok () state
runCmd Dry state ReadGuess = pure OutOfFuel
runCmd (More fuel) state ReadGuess = do putStr "Guess: "
                                        guess <- getLine
                                        case unpack guess of
                                             [x] => case isAlpha x of 
                                                         True => ok (toUpper x) state
                                                         False => do putStrLn "Invalid input"
                                                                     runCmd fuel state ReadGuess
                                             _ => do putStrLn "Invalid input"
                                                     runCmd fuel state ReadGuess

run : Fuel -> Game instate -> GameLoop ty instate outstate_fn -> IO (GameResult ty outstate_fn) 
run Dry _ _ = pure OutOfFuel 
run (More fuel) st (cmd >>= next) = do OK cmdRes newSt <- runCmd fuel st cmd
                                            | OutOfFuel => pure OutOfFuel
                                       run fuel newSt (next cmdRes)
                                       
run (More fuel) st Exit = ok () st

%default partial 
forever : Fuel
forever = More forever

main : IO ()
main = do run forever GameStart hangman
          pure ()
