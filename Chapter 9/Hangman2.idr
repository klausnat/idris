import Data.Vect

data WordState : (guesses : Nat) -> (letters : Nat) -> Type where
     MkWordState : (word : String) -> (lettersLeft: Vect letters Char) -> WordState guesses letters
     

data ValidInput : List Char -> Type where
     Letter : (c : Char) -> ValidInput [c]

          
data Finished : Type where
     Won : (st : WordState (S guesses) Z) -> Finished
     Lost : (st : WordState Z (S letters)) -> Finished



notInNil : ValidInput [] -> Void
notInNil (Letter _) impossible

notMoreThanOne : ValidInput (x :: (y :: xs)) -> Void
notMoreThanOne (Letter _) impossible

isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput [] = No notInNil
isValidInput (x :: []) = Yes (Letter x)
isValidInput (x :: (y :: xs)) = No notMoreThanOne


isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput (unpack s)

readGuess : IO (x ** ValidInput x)
readGuess = do putStr "Guess: "
               str <- getLine 
               case isValidString (toUpper str) of
                    Yes prf => pure (_ ** prf) 
                    No contra => do putStrLn "Invalid Input"
                                    readGuess


processGuess : WordState (S guesses) (S letters) -> Char -> 
               Either (WordState guesses (S letters)) (WordState (S guesses) letters) 
processGuess (MkWordState word lettersLeft) c 
  = case isElem c lettersLeft of
         Yes prf => Right (MkWordState word (dropElem lettersLeft prf))
         No contra => Left (MkWordState word lettersLeft) 

game : WordState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} st = do (_ ** Letter letter) <- readGuess
                                 case processGuess st letter of
                                      Left l => do putStrLn "Wrong"
                                                   case guesses of
                                                        Z => pure (Lost l)
                                                        S k => game l 
                                      Right r => do putStrLn "Right!" 
                                                    case letters of 
                                                         Z => pure (Won r)
                                                         S k => game r
                                      
main : IO ()
main = do result <- game {guesses = 2} (MkWordState "Test" ['T', 'E', 'S'])
          case result of 
               Won st => putStrLn "You Win!"
               Lost (MkWordState word lettersLeft) => putStrLn ("You Lose, word was" ++ word)
               

 
 
