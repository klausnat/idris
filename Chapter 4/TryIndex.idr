import Data.Vect

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} i xs = case integerToFin i n of
                         Nothing  => Nothing
                         Just idx => Just (index idx xs)

-- Exercises 3,4: write a function vectTake

vectTake : (m : Fin n) -> Vect n a -> Vect (finToNat m) a
vectTake FZ xs = []
vectTake (FS x) (y :: xs) = y :: vectTake x xs

