import Data.Vect
import Debug.Trace
%default total
data Fuel = Dry | More (Lazy Fuel)

data GameState : Type where
     Running : (guesses : Nat) -> (letters : Nat) -> GameState
     NotRunning : GameState

data GuessResult = Correct | Incorrect

letters : String -> List Char
letters x = nub (map toUpper (unpack x))

data GameCmd : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
     NewGame : (word : String) -> GameCmd () NotRunning (const (Running 6 (length (letters word))))
     Won : GameCmd () (Running (S guesses) 0) (const NotRunning)
     Lost : GameCmd () (Running 0 (S letters)) (const  NotRunning)
     Pure : (res : a) -> GameCmd a (state_fn res) state_fn
     (>>=) : GameCmd a state1 state2_fn ->
             ((res : a) -> GameCmd b (state2_fn res) state3_fn) ->
             GameCmd b state1 state3_fn
     Guess : GameCmd Char state (const state)
     ProcessGuess : Char -> GameCmd GuessResult (Running (S guesses) (S letters)) (\res => case res of 
                                                            Correct => Running (S guesses) letters
                                                            Incorrect => Running guesses (S letters))
     Message : String -> GameCmd () state (const state)
     ShowState : GameCmd String state (const state) 

namespace Loop
  data GameLoop : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
       (>>=) : GameCmd a state1 state2_fn ->
               ((res : a) -> Inf (GameLoop b (state2_fn res) state3_fn)) ->
               GameLoop b state1 state3_fn
       Exit : GameLoop () NotRunning (const NotRunning)

gameloop : GameLoop () (Running (S guesses) (S letters)) (const NotRunning)
gameloop {guesses} {letters} = do ShowState
                                  g <- Guess
                                  r <- ProcessGuess g 
                                  case r of Correct => case letters of 
                                                            (S ltrs) => do Message "Correct!"
                                                                           gameloop
                                                            Z => do Message "You win!"
                                                                    Won
                                                                    Exit
                                            Incorrect => case guesses of 
                                                              (S gss) => do Message "Wrong!"
                                                                            gameloop
                                                              Z => do Message "Wrong"
                                                                      Lost
                                                                      Exit

hangman : GameLoop () NotRunning (const NotRunning)
hangman = do NewGame "testing" 
             gameloop

data Game : GameState -> Type where
     GameLost : (word : String) -> Game NotRunning
     GameWon : (word : String) -> Game NotRunning
     InProcess : (word : String) -> 
                 (guesses : Nat) -> 
                 (missing : Vect ltrs Char) -> Game (Running guesses ltrs)
     StartGame : Game NotRunning  

Show (Game g) where
  show (GameLost word) = "Game Lost, word was: " ++ word
  show (GameWon word) = "Game Won, word was: " ++ word
  show (InProcess word guesses missing) = "Guesses left: " ++ show guesses ++ "\n Word to guess: " ++
                                                 pack ( map showLetter (unpack word)) where
                                                  showLetter : Char -> Char
                                                  showLetter c = case isElem (toUpper c) missing of
                                                                      Yes prf => '-'
                                                                      No contra => c
  show StartGame = "Starting Game"


data GameResult : (ty : Type) -> (ty -> GameState) -> Type where
     OK : (res : a) -> Game (outstate_fn res) -> GameResult a outstate_fn
     OutOfFuel : GameResult res outstate_fn

ok : (res : a) -> Game (outstate_fn res) -> IO (GameResult a outstate_fn)
ok res st  = pure (OK res st)

runCmd : Fuel -> Game instate -> GameCmd a instate outstate_fn -> IO (GameResult a outstate_fn)
runCmd Dry instate z = pure OutOfFuel
runCmd (More x) instate (NewGame word) = ok () (InProcess word _ (fromList (letters word)))
runCmd (More x) (InProcess word _ _) Won = ok () (GameWon word)
runCmd (More x) (InProcess word _ _) Lost = ok () (GameLost word)
runCmd (More x) instate (Pure res) = ok res instate
runCmd (More fuel) instate (cmd >>= next) = do OK cmdRes newSt <- runCmd fuel instate cmd
                                                               | _ => pure OutOfFuel
                                               runCmd fuel newSt (next cmdRes)                
runCmd (More fuel) instate Guess = do putStr "Guess: "
                                      c <- getLine
                                      case unpack c of
                                        [x] => case isAlpha x of
                                                    True => ok (toUpper x) instate
                                                    False => do putStrLn "Invalid input"
                                                                runCmd fuel instate Guess
                                        _ => do putStrLn "Invalid input"
                                                runCmd fuel instate Guess
                                                
runCmd (More x) (InProcess word _ missing) (ProcessGuess char) 
  = do case isElem char missing of
            Yes prf => do ok Correct (InProcess word _ (dropElem missing prf))
            No contra => do ok Incorrect (InProcess word _ missing) 
  
runCmd (More x) instate (Message y) = do putStrLn y
                                         ok () instate
runCmd (More x) instate ShowState = do putStrLn (show instate)
                                       ok (show instate) instate

run : Fuel -> Game instate -> GameLoop a instate outstate_fn -> IO (GameResult a outstate_fn)
run Dry state cmd = pure OutOfFuel
run (More fuel) state (cmd >>= next) = do OK cmdRes newSt <- runCmd fuel state cmd
                                                          | _ => pure OutOfFuel
                                          run fuel newSt (next cmdRes)
run fuel state Exit = ok () state

%default partial
forever : Fuel
forever = More forever

main : IO ()
main = do run forever StartGame hangman
          pure ()
