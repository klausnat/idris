import Data.Vect

%default total

data StackCmd : Type -> Nat -> Nat -> Type where
     Push : Integer -> StackCmd () height (S height)
     Pop : StackCmd Integer (S height) height 
     Top : StackCmd Integer (S height) (S height)
     
     GetStr : StackCmd String height height
     PutStr : String -> StackCmd () height height 
     
     Pure : ty -> StackCmd ty height height
     (>>=) : StackCmd a height1 height2 -> (a -> StackCmd b height2 height3) -> StackCmd b height1 height3
     
testAdd : StackCmd Integer 0 0     
testAdd = do Push 10
             Push 20
             val1 <- Pop
             val2 <- Pop
             Pure (val1 + val2)
             
runStack : (stk : Vect inHeight Integer) -> StackCmd ty inHeight outHeight -> IO (ty, Vect outHeight Integer)
runStack stk (Push x) = pure ((), x :: stk)
runStack (x :: xs) Pop = pure (x,xs)
runStack (x :: xs) Top = pure (x, x :: xs)

runStack stk GetStr = do x <- getLine
                         pure (x, stk)
runStack stk (PutStr str) = do putStr str
                               pure ((), stk)

runStack stk (Pure x) = pure (x, stk)
runStack stk (cmd >>= next) = do (cmdRes, newStk) <- runStack stk cmd 
                                 runStack newStk (next cmdRes)

data StackIO : Nat -> Type where
     Do : StackCmd a height1 height2 -> (a -> Inf (StackIO height2)) -> StackIO height1

namespace StackDo
  (>>=) : StackCmd a height1 height2 -> (a -> Inf (StackIO height2)) -> StackIO height1
  (>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

data StkInput = Number Integer | Add

strToInput : String -> Maybe StkInput
strToInput "" = Nothing
strToInput "add" = Just Add
strToInput x = if all isDigit (unpack x) then Just (Number (cast x))
                                         else Nothing

-- add the first two elements and put result back into the stack
doAdd : StackCmd () (S (S height)) (S height)
doAdd = do val1 <- Pop
           val2 <- Pop
           Push (val1 + val2)


mutual 
  tryAdd : StackIO height
  tryAdd {height = S (S h)} = do doAdd
                                 res <- Top
                                 PutStr ("Result: " ++ show res ++ "\n")
                                 stackCalc
                                 
  tryAdd = do PutStr "Fewer than two items on the stack \n"
              stackCalc
  
  stackCalc : StackIO height
  stackCalc = do PutStr "> "
                 input <- GetStr
                 case strToInput input of
                      Nothing => do PutStr "Invalid input \n"
                                    stackCalc 
                      Just (Number x) => do Push x
                                            stackCalc
                      Just Add => tryAdd
  
partial
forever : Fuel
forever = More forever  
      

run : Fuel -> Vect height Integer -> StackIO height -> IO ()                      
run Dry stk p = pure ()
run (More fuel) stk (Do y f) = do (res, newStack) <- runStack stk y
                                  run fuel newStack (f res)
partial
main : IO ()
main = run forever [] stackCalc
