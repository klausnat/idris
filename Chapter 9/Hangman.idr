import Data.Vect

data WordState : (guesses : Nat) -> (letters : Nat) -> Type where
     MkState : (word : String) -> (lettersLeft : Vect letters Char) -> WordState guesses letters


data Finished : Type where
     Lost : (game : WordState Z (S letters)) -> Finished 
     Won : (game : WordState (S guesses) Z) -> Finished

data ValidInput : List Char -> Type where
     Letter : (c : Char) -> ValidInput [c]

notNil : ValidInput [] -> Void
notNil (Letter _) impossible

notTwo : ValidInput (x :: (y :: xs)) -> Void
notTwo (Letter _) impossible

isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput [] = No notNil
isValidInput (x :: []) = Yes (Letter x)
isValidInput (x :: (y :: xs)) = No notTwo

isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput (unpack s)

readGuess : IO (x ** ValidInput x)
readGuess = do putStr "Enter a guess: "
               x <- getLine
               case isValidString (toUpper x) of
                    Yes prf => pure (_ ** prf)
                    No contra => do putStrLn "Invalid Guess"
                                    readGuess
-- data Either a b = Left a | Right b

processGuess : (letter : Char) -> 
               WordState (S guesses) (S letters) ->
               Either (WordState guesses (S letters))
                      (WordState (S guesses) letters)
                                                            
processGuess letter (MkState word lettersLeft) = case isElem letter lettersLeft of
                                                      Yes prf => Right (MkState word (dropElem lettersLeft prf))
                                                      No contra => Left (MkState word lettersLeft)

game : (st : WordState (S guesses) (S letters)) -> IO Finished          
game {guesses} {letters} st = do (_ ** Letter x) <- readGuess
                                 case processGuess x st of
                                      Right r => do putStrLn "Right!"
                                                    case letters of 
                                                      Z => pure (Won r)
                                                      S k => game r
                                      Left l => do putStrLn "Wrong!"
                                                   case guesses of 
                                                        Z => pure (Lost l)
                                                        S k => game l 
             


main : IO ()
main = do result <- game {guesses = 2} (MkState "Test" ['T', 'E', 'S'] )
          case result of 
               Lost (MkState word lettersLeft) => putStrLn ("You lose! Word was: " ++ word)
               Won game => putStrLn "You win!"


