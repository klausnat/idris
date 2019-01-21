-- started 8:18
import Data.Vect
%default total
data Fuel = Dry | More (Lazy Fuel)

data GameState : Type where
     Running : (guesses : Nat) -> (letters : Nat) -> GameState
     NotRunning : GameState

data GuessResult = Correct | Incorrect

letters : String -> List Char
letters str = nub ( map toUpper (unpack str))          

data GameCmd : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
     Won : GameCmd () (Running (S guesses) 0) (const NotRunning)
     Lost : GameCmd () (Running 0 (S letters)) (const NotRunning)
     StartGame : (word : String) -> GameCmd () NotRunning (const (Running 6 (length (letters word))))
     ShowState : GameCmd String state (const state)
     Message : String -> GameCmd () state (const state)
     Pure : (res : a) -> GameCmd a (state_fn res) state_fn
     (>>=) : GameCmd a state1 state2_fn ->
             ((res : a) -> GameCmd b (state2_fn res) state3_fn) ->
             GameCmd b state1 state3_fn
     Guess : GameCmd Char state (const state)
     ProcessGuess : Char -> GameCmd GuessResult (Running (S guesses) (S letters)) 
                            (\res => case res of Correct => Running (S guesses) letters
                                                 Incorrect => Running guesses (S letters)
                            )

namespace Loop
  data GameLoop : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
       (>>=) : GameCmd a state1 state2_fn ->
               ((res : a) -> Inf (GameLoop b (state2_fn res) state3_fn)) ->
               GameLoop b state1 state3_fn
       Exit : GameLoop () NotRunning (const NotRunning)

gameloop : GameLoop () (Running (S guesses) (S letters)) (const NotRunning)
gameloop {guesses} {letters} = do ShowState
                                  g <- Guess
                                  c <- ProcessGuess g
                                  case c of Correct => case letters of (S ltrs) => do Message "Correct!"
                                                                                      gameloop
                                                                       Z => do Won
                                                                               ShowState
                                                                               Exit
                                            Incorrect => case guesses of (S gss) => do Message "Wrong!"
                                                                                       gameloop
                                                                         Z => do Lost
                                                                                 ShowState
                                                                                 Exit


hangman : GameLoop () NotRunning (const NotRunning)

hangman = do StartGame "testing"
             gameloop

data Game : GameState -> Type where
     GameLost : (word : String) -> Game NotRunning
     GameWon : (word : String) -> Game NotRunning
     GameStart : Game NotRunning
     InProgress : (word : String) -> (guesses : Nat) -> (missing : Vect ltrs Char) -> Game (Running guesses ltrs)
     
Show (Game g) where     
  show (GameLost word) = "Game Lost, word was: " ++ word
  show (GameWon word) = "Game Won, word was: " ++ word
  show GameStart = "Starting game."
  show (InProgress word guesses missing) = "Guesses left: " ++ show guesses ++ "\n" ++
                                           pack ( map showLetter (unpack word) ) where
                                           showLetter : Char -> Char
                                           showLetter x = if elem (toUpper x) missing then '-' else x
 

data GameResult : (ty : Type) -> (ty -> GameState) -> Type where
     OK : (res : a) -> Game (outstate_fn res) -> GameResult a outstate_fn
     OutOfFuel : GameResult res outstate_fn
     
ok : (res : a) -> Game (outstate_fn res) -> IO (GameResult a outstate_fn)
ok res st = pure (OK res st)

runCmd : Fuel -> Game instate -> GameCmd a instate outstate_fn -> IO (GameResult a outstate_fn)
runCmd Dry y z = pure OutOfFuel
runCmd (More fuel) (InProgress word _ _) Won = ok () (GameWon word)
runCmd (More fuel) (InProgress word _ _) Lost = ok () (GameLost word)
runCmd (More fuel) y (StartGame word) = ok () (InProgress word _ (fromList (letters word)))
runCmd (More fuel) y ShowState = do putStrLn (show y)
                                    ok (show y) y
runCmd (More fuel) y (Message x) = do putStrLn x
                                      ok () y
runCmd (More fuel) y (Pure x) = ok x y
runCmd (More fuel) instate (cmd >>= next) = do (OK cmdRes newSt) <- runCmd fuel instate cmd
                                                                 | _ => pure OutOfFuel
                                               runCmd fuel newSt (next cmdRes)
runCmd (More fuel) y Guess = do putStr "Guess: "
                                g <- getLine
                                case unpack g of
                                  [x] => case isAlpha x of 
                                              True => ok (toUpper x) y
                                              False => do putStrLn "Invalid input"
                                                          runCmd fuel y Guess
                                  _ => do putStrLn "Invalid input"
                                          runCmd fuel y Guess
runCmd (More fuel) (InProgress word _ missing) (ProcessGuess x) = do case isElem (toUpper x) missing of
                                                                                 Yes prf => ok Correct (InProgress word _ (dropElem missing prf)) 
                                                                                 No contra => ok Incorrect (InProgress word _ missing)

run : Fuel -> Game instate -> GameLoop a instate outstate_fn -> IO (GameResult a outstate_fn)
run Dry y z = pure OutOfFuel
run (More fuel) y (cmd >>= next) = do (OK cmdRes newSt) <- runCmd fuel y cmd 
                                                      | OutOfFuel => pure OutOfFuel
                                      run fuel newSt (next cmdRes)   
run (More x) y Exit = ok () y

%default partial
forever : Fuel
forever = More forever
main : IO ()
main = do run forever GameStart hangman 
          pure ()

