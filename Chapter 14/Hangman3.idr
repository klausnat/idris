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
     GetState : GameCmd () state (const state)
     (>>=) : GameCmd a state1 state2_fn ->
             ((res : a) -> GameCmd b (state2_fn res) state3_fn) ->
             GameCmd b state1 state3_fn
     Pure : (res : ty) -> (GameCmd ty (state2_fn res) state2_fn) 
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
runCmd (More x) y (StartGame word) = ok () 
                                        (InProcess (toUpper word) _ (fromList (letters word)))
runCmd (More x) (InProcess word _ missing) Lost = ok () (GameLost word)
runCmd (More x) (InProcess word _ missing) Won = ok () (GameWon word)
runCmd (More x) y (Message z) = do putStr z
                                   ok () y
runCmd (More x) y GetState = do printLn y
                                ok () y
runCmd (More x) y (cmd >>= next) = do OK cmdNew stNew <- runCmd x y cmd
                                                      | _ => pure OutOfFuel
                                      runCmd x stNew (next cmdNew)
runCmd (More x) y (Pure res) = ok res y
runCmd (More x) (InProcess word _ missing) (Guess c) = case isElem c missing of
                                                            Yes prf => ok Correct (InProcess word _ (dropElem missing prf))
                                                            No contra => ok Incorrect (InProcess word _ missing)
runCmd (More x) y GetGuess = do putStr "Guess: "
                                c <- getLine
                                case unpack c of
                                     [z] => case isAlpha z of
                                                 True => ok (toUpper z) y
                                                 False => do putStrLn "Invalid input"
                                                             runCmd x y GetGuess
                                     _ => do putStrLn "Invalid input"
                                             runCmd x y GetGuess
                                      
run : Fuel -> Game instate -> GameLoop ty instate outstate_fn -> IO (GameResult ty outstate_fn)
run Dry y z = pure OutOfFuel
run (More x) y (z >>= f) = do OK res newSt <- runCmd x y z
                                           | _ => pure OutOfFuel
                              run x newSt (f res)   
run (More x) y Exit = ok () y


%default partial

forever : Fuel
forever = More forever
main : IO ()
main = do run forever GameStart hangman
          pure ()
