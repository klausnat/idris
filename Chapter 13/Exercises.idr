-- 1. Change the RingBell operation so that it works in any state, rather than only when the door is closed.

data DoorState = DoorOpen | DoorClosed

data DoorCmd : Type -> DoorState -> DoorState -> Type where
     Open : DoorCmd () DoorClosed DoorOpen
     Close : DoorCmd () DoorOpen DoorClosed
     RingBell : DoorCmd () state state
     (>>=) : DoorCmd a state1 state2 -> (a -> DoorCmd b state2 state3) -> DoorCmd b state1 state3
     
doorProg : DoorCmd () DoorClosed DoorClosed
 
doorProg = do RingBell
              Open
              RingBell
              Close

-- 2. The following (incomplete) type defines a command for a guessing game, where the input and output states are the number of remaining guesses allowed:

data GuessCmd : Type -> Nat -> Nat -> Type where
     Try : Integer -> GuessCmd Ordering (S guesses) guesses
     PureGuess : ty -> GuessCmd ty state state
     Bind : GuessCmd a state1 state2 -> (a -> GuessCmd b state2 state3) -> GuessCmd b state1 state3
     
namespace GuessDo
  (>>=) : GuessCmd a state1 state2 -> (a -> GuessCmd b state2 state3) -> GuessCmd b state1 state3
  (>>=) = Bind

threeGuesses : GuessCmd () 3 0
threeGuesses = do Try 10
                  Try 20
                  Try 15
                  PureGuess ()


{- shouldn't type-check

noGuesses : GuessCmd () 0 0
noGuesses = do Try 10
               PureGuess ()
-}

-- 3. The following type defines the possible states of matter

data Matter = Solid | Liquid | Gas 

data MatterCmd : Type -> Matter -> Matter -> Type where
     Melt : MatterCmd () Solid Liquid
     Boil : MatterCmd () Liquid Gas
     Condense : MatterCmd () Gas Liquid
     Freeze : MatterCmd () Liquid Solid
     Do : MatterCmd a state1 state2 -> (a -> MatterCmd b state2 state3) -> MatterCmd b state1 state3
     Pure : ty -> MatterCmd ty matter matter
   
namespace MatterDo
  (>>=) : MatterCmd a state1 state2 -> (a -> MatterCmd b state2 state3) -> MatterCmd b state1 state3
  (>>=) = Do
      
iceSteam : MatterCmd () Solid Gas
iceSteam = do Melt
              Boil

steamIce : MatterCmd () Gas Solid
steamIce = do Condense
              Freeze

-- should not type check
{-
overMelt : MatterCmd () Solid Gas
overMelt = do Melt
              Melt
-}

