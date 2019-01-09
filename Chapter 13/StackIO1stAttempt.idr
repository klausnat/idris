import Data.Vect

%default total

data Fuel = Dry | More (Lazy Fuel)

partial 
forever : Fuel
forever = More forever

data StackCmd : Type -> Nat -> Nat -> Type where
     Push : Integer -> StackCmd () height (S height)
     Pop : StackCmd Integer (S height) height
     Top : StackCmd Integer (S height) (S height)
     
     PutStr : String -> StackCmd () height height
     GetStr : StackCmd String height height
     
     Pure : ty -> StackCmd ty height height
     (>>=) : StackCmd a height1 height2 -> (a -> StackCmd b height2 height3) -> StackCmd b height1 height3
     
data UsrInput = Number Integer | Add

parseInput : String -> Maybe UsrInput     
parseInput "" = Nothing
parseInput "add" = Just Add
parseInput x = if all isDigit (unpack x) then Just (Number (cast x)) else Nothing



data StackIO : Nat -> Type where
     Do : StackCmd a initHeight outHeight -> (a -> Inf (StackIO outHeight)) -> StackIO initHeight

namespace StackDo
  (>>=) : StackCmd a initHeight outHeight -> (a -> Inf (StackIO outHeight)) -> StackIO initHeight
  (>>=) = Do


runStack : (stk : Vect initHeight Integer) -> StackCmd a initHeight outHeight -> IO (a, Vect outHeight Integer)
runStack stk (Push x) = pure ((), x :: stk)
runStack (x :: xs) Pop = pure (x, xs)
runStack (x :: xs) Top = pure (x, x :: xs)
runStack stk (PutStr x) = do putStr x
                             pure ((), stk)
runStack stk GetStr = do res <- getLine
                         pure (res, stk)
runStack stk (Pure x) = pure (x, stk)
runStack stk (x >>= f) = do (res, newStack) <- runStack stk x
                            runStack newStack (f res) 


doAdd : StackCmd () (S (S height)) (S height) 
doAdd = do val1 <- Pop
           val2 <- Pop
           Push (val1 + val2)


mutual 
  tryAdd : StackIO height
  tryAdd {height = S (S height)} = do doAdd
                                      val <- Top
                                      PutStr (show val ++ "\n")
                                      calc
  tryAdd = do PutStr "Too few elements on a stack\n"
              calc
  
  calc : StackIO height
  calc = do PutStr "> "
            res <- GetStr
            case parseInput res of
                 Nothing => do PutStr "Invalid input\n"
                               calc
                 Just Add => tryAdd
                 Just (Number x) => do Push x
                                       calc
                     
                 
  

run : Fuel -> Vect height Integer -> StackIO height -> IO ()
run Dry xs y = pure ()
run (More fuel) stk (Do y f) = do (res, newStack) <- runStack stk y
                                  run fuel newStack (f res)

partial
main : IO ()
main = run forever [] calc
