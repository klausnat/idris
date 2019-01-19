import Data.Vect
%default total

data GameState : Type where
     Running : (guesses : Nat) -> (letters : Nat) -> GameState
     NotRunning : GameState 

data GuessResult = Correct | Incorrect

letters : String -> List Char
letters str = nub $ map toUpper (unpack str)

data GameCmd : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
     GameStart : (word : String) -> GameCmd () NotRunning (const (Running 6 (length (letters word))))
     Won : GameCmd () (Running (S guesses) 0) (const NotRunning)
     Lost : GameCmd () (Running 0 (S letters) ) (const NotRunning)
     ShowState : GameCmd String state (const state) 
     Message : String -> GameCmd () state (const state)
     Pure : (res : a) -> GameCmd a (gamestate_fn res) gamestate_fn
     (>>=) : GameCmd a state1 state2_fn ->
             ((res : a) -> GameCmd b (state2_fn res) state3_fn) ->
             GameCmd b state1 state3_fn
     Guess : GameCmd Char state (const state)
     ProcessGuess : Char -> GameCmd GuessResult 
                                    (Running (S guesses) (S letters)) 
                                    (\res => case res of Correct => Running (S guesses) letters
                                                         Incorrect => Running guesses (S letters))

data Fuel = Dry | More (Lazy Fuel)

namespace Loop
  data GameLoop : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
       (>>=) : GameCmd a state1 state2_fn ->
               ((res : a) -> Inf (GameLoop b (state2_fn res) state3_fn)) ->
               GameLoop b state1 state3_fn
       Exit : GameLoop () NotRunning (const NotRunning)

gameloop : GameLoop () (Running (S guesses) (S letters)) (const NotRunning) 
gameloop {guesses} {letters} = do ShowState
                                  g <- Guess
                                  x <- ProcessGuess g
                                  case x of Correct => case letters of (S ltrs) => do Message "Right!"
                                                                                      gameloop
                                                                       Z => do Message "Right! You Win"
                                                                               Won
                                                                               Exit
                                            Incorrect => case guesses of (S gs) => do Message "Wrong!"
                                                                                      gameloop
                                                                         Z => do Message "Wrong! You Lost"
                                                                                 Lost 
                                                                                 Exit

hangman : GameLoop () NotRunning (const NotRunning)
hangman = do GameStart "testing"
             gameloop


data Game : GameState -> Type where
     StartGame : Game NotRunning
     LostGame : (word : String) -> Game NotRunning
     WonGame : (word : String) -> Game NotRunning
     InProgress : (word : String) ->
                  (guesses : Nat) -> 
                  (missing : Vect ltrs Char) -> Game (Running guesses ltrs)
                  
Show (Game g) where
  show StartGame = "Starting game"
  show (LostGame word) = "Game Lost, word was: " ++ word
  show (WonGame word) = "Game Won, word was: " ++ word
  show (InProgress word guesses missing) = "Guesses left: " ++ show guesses ++ "\n" ++
                                           pack (map showLetter (unpack word)) where
                                             showLetter : Char -> Char
                                             showLetter c = if elem (toUpper c) missing then '-' else c
  


data GameResult : (ty : Type) -> (ty -> GameState) -> Type where
     OK : (res : a) -> Game (outstate_fn res) -> GameResult a outstate_fn
     OutOfFuel : GameResult res outstate_fn

ok : (res : a) -> Game (outstate_fn res) -> IO (GameResult a outstate_fn)
ok res st = pure (OK res st)

runCmd : Fuel -> Game instate -> GameCmd a instate outstate_fn -> IO (GameResult a outstate_fn)
runCmd Dry y z = pure OutOfFuel
runCmd (More fuel) y (GameStart word) = ok () (InProgress word _ (fromList (letters word)))
runCmd (More fuel) (InProgress word _ _) Won = ok () (WonGame word)
runCmd (More fuel) (InProgress word _ _) Lost = ok () (LostGame word)
runCmd (More fuel) y ShowState = do putStrLn (show y)
                                    ok (show y) y

runCmd (More fuel) y (Message x) = do putStrLn x
                                      ok () y
runCmd (More fuel) y (Pure res) = ok res y
runCmd (More fuel) y (cmd >>= next) = do OK cmdRes newSt <- runCmd fuel y cmd
                                                         | _ => pure OutOfFuel
                                         runCmd fuel newSt (next cmdRes)
runCmd (More fuel) y Guess = do putStrLn "Guess: "
                                g <- getLine
                                case unpack g of 
                                     [x] => case isAlpha x of True => ok (toUpper x) y
                                                              False => do putStrLn "Invalid input"
                                                                          runCmd fuel y Guess
                                     _ => do putStrLn "Invalid input"
                                             runCmd fuel y Guess
runCmd (More fuel) (InProgress word _ missing) (ProcessGuess x) = case isElem x missing of 
                                                                              Yes prf => ok Correct (InProgress word _ (dropElem missing prf))
                                                                              No contra => ok Incorrect (InProgress word _ missing)

run : Fuel -> Game instate -> GameLoop a instate outstate_fn -> IO (GameResult a outstate_fn)
run Dry y z = pure OutOfFuel
run (More fuel) state (cmd >>= next) = do OK cmdRes newSt <- runCmd fuel state cmd
                                                          | OutOfFuel => pure OutOfFuel
                                          run fuel newSt (next cmdRes)
run (More fuel) state Exit = ok () state

%default partial
forever : Fuel
forever = More forever

main : IO ()
main = do run forever StartGame hangman
          pure ()
