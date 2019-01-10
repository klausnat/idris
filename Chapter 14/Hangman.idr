import Data.Vect

data WordState : (guesses : Nat) -> (letters : Nat) -> Type where
     MkWordState : (word : String) 
                 -> (missing : Vect letters Char)
                 -> WordState guesses_remaining letters

data Finished : Type where
     Lost : (game : WordState 0 (S letters)) -> Finished  
     Won : (game : WordState (S guesses) 0) -> Finished
     
game : WordState (S guesses) (S letters) -> IO Finished     
