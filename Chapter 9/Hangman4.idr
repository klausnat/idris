import Data.Vect

data WordState : (guesses : Nat) -> (letters : Nat) -> Type where
     MkWordState : (word : String) -> (Vect letters Char) -> WordState guesses letters
     
data Finished : Type where
     Won : WordState guesses Z -> Finished
     Lost : WordState Z letters -> Finished

data ValidInput : (cs : List Char) -> Type where
     Letter : (c : Char) -> ValidInput [c]
     
notInNIl : ValidInput [] -> Void
notInNIl (Letter _) impossible

notTwo : ValidInput (x :: (y :: xs)) -> Void
notTwo (Letter _) impossible

isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput [] = No notInNIl
isValidInput (x :: []) = Yes (Letter x)
isValidInput (x :: (y :: xs)) = No notTwo

isValidString : (str : String) -> Dec (ValidInput (unpack str)) 
isValidString str = isValidInput (unpack str)

readGuess : IO (c ** ValidInput c)
readGuess = do putStr "Guess: "
               x <- getLine
               case isValidString (toUpper x) of
                    Yes prf => pure (_ ** prf)
                    No contra => do  putStrLn "Invalid input"
                                     readGuess

processGuess : WordState (S guesses) (S letters) -> Char -> Either (WordState guesses (S letters)) (WordState (S guesses) (letters))
processGuess (MkWordState word missing) letter = case isElem letter missing of
                                                 Yes prf => Right (MkWordState word (dropElem missing prf))
                                                 No contra => Left (MkWordState word missing)
                                                 
game : WordState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} st = do (_ ** Letter letter) <- readGuess
                                 case processGuess st letter of
                                      Left l => do putStrLn "Wrong!"
                                                   case guesses of
                                                        Z => pure (Lost l)
                                                        S k => game l
                                      Right r => do putStrLn "Right!"
                                                    case letters of
                                                         S k => game r
                                                         Z => pure (Won r)

main : IO ()                                                 
main = do result <- game {guesses = 2} (MkWordState "Test" ['T','E','S'])
          case result of
               Won st => putStrLn "You Win"
               Lost (MkWordState word xs) => putStrLn ("You lose, word was" ++ word)
               
               
