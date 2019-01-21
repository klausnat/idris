import Data.Vect
%default total

data GameState : Type where
     Running : (guesses : Nat) -> (letters : Nat) -> GameState
     NotRunning : GameState
     
data GuessResult = Correct | Incorrect

letters : String -> List Char
letters str = nub (map toUpper (unpack str))

data GameCmd : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
     Won : GameCmd () (Running (S guesses) 0 ) (const NotRunning)
     Lost : GameCmd () (Running 0 (S letters)) (const NotRunning)
     NewGame : (word : String) -> GameCmd () (NotRunning) (const (Running 6 (length (letters word))))
     Pure : (res : a) -> GameCmd a (outstate_fn res) outstate_fn
     (>>=) : GameCmd a state1 state2_fn ->
             ((res : a) -> GameCmd b (state2_fn res) state3_fn) ->
             GameCmd b state1 state3_fn
     Message : String -> GameCmd () state (const state)
     ShowState : GameCmd String state (const state)
     Guess : GameCmd Char state (const state)
     ProcessGuess : Char -> GameCmd GuessResult 
                            (Running (S guesses) (S letters)) 
                            (\res => case res of Correct => (Running (S guesses) letters)
                                                 Incorrect => (Running guesses (S letters))
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
                                  r <- ProcessGuess g
                                  case r of Correct => case letters of (S ltrs) => do Message "Correct."
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
hangman = do NewGame "testing"
             gameloop
             
data Game : GameState -> Type where
     GameLost : (word : String) -> Game NotRunning
     GameWon : (word : String) -> Game NotRunning
     GameStart : Game NotRunning
     InProgress : (word : String) -> (guesses : Nat) -> (missing : Vect ltrs Char) -> Game (Running guesses ltrs)
   
Show (Game g) where   
  show (GameLost word) = "Game Lost, word was: " ++ word
  show (GameWon word) = "Game won, word was: " ++ word
  show GameStart  = "Game not running"
  show (InProgress word guesses missing) = "guesses left: " ++ show guesses ++ "\n"
                                           ++ pack (map showLetter (unpack word)) where
                                              showLetter : Char -> Char 
                                              showLetter x = if elem (toUpper x) missing then '-' else x

data GameResult : (ty : Type) -> outstate_fn -> Type where
     OK : (res : ty) -> Game (outstate_fn res) -> GameResult ty outstate_fn
     OutOfFuel : GameResult ty outstate_fn

ok : (res : ty) -> Game (outstate_fn res) -> IO (GameResult ty outstate_fn)
ok res st = pure (OK res st)

data Fuel = Dry | More (Lazy Fuel)

runCmd : Fuel -> Game instate -> GameCmd a instate outstate_fn -> IO (GameResult a outstate_fn)
runCmd Dry y z = pure OutOfFuel
runCmd (More fuel) (InProgress word _ _) Won = ok () (GameWon word)
runCmd (More fuel) (InProgress word _ _) Lost = ok () (GameLost word)
runCmd (More fuel) instate (NewGame word) = ok () (InProgress word _ (fromList (letters word)))
runCmd (More fuel) instate (Pure res) = ok res instate
runCmd (More fuel) instate (cmd >>= next) = do (OK cmdRes newSt) <- runCmd fuel instate cmd
                                                                 | _ => pure OutOfFuel
                                               runCmd fuel newSt (next cmdRes)
runCmd (More fuel) instate (Message x) = do putStrLn x
                                            ok () instate
runCmd (More fuel) instate ShowState = do putStrLn (show instate) 
                                          ok (show instate) instate
runCmd (More fuel) instate Guess = do putStr "Guess: "
                                      x <- getLine
                                      case unpack x of 
                                           [c] => case isAlpha c of
                                                       True => ok (toUpper c) instate
                                                       False => do putStrLn "Invalid input"
                                                                   runCmd fuel instate Guess
                                           _ => do putStrLn "Invalid input"
                                                   runCmd fuel instate Guess
runCmd (More fuel) (InProgress word _ missing) (ProcessGuess x) 
  = case isElem x missing of
         Yes prf => ok Correct (InProgress word _ (dropElem missing prf))
         No contra => ok Incorrect (InProgress word _ missing) 
         
run : Fuel -> Game instate -> GameLoop a instate outstate_fn -> IO (GameResult a outstate_fn)
run Dry y z = pure OutOfFuel
run (More fuel) instate (cmd >>= next) = do (OK cmdRes newSt) <- runCmd fuel instate cmd 
                                                              | _ => pure OutOfFuel
                                            run fuel newSt (next cmdRes)   
run (More fuel) instate Exit = ok () instate

%default partial
forever : Fuel
forever = More forever

main : IO ()
main = do run forever GameStart hangman
          pure ()

 
