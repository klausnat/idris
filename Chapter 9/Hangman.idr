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


game : (WordState (S guesses) (S letters)) -> IO Finished          
game x = ?ss


main : IO ()
-- main = game      


