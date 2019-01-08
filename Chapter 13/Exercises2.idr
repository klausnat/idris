import Data.Vect

-- 1. Add user commands to the stack-based calculator for substract and multiply
-- 2. Add a negate user command to the stack-based calculator for negating the top item on the stack
-- 3. Add a discard user command that removes the top item from the stack
-- 4. Add a duplicate user command that duplicates the top item on the stack

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

data StkInput = Number Integer | Add | Substract | Multiply | Negate

strToInput : String -> Maybe StkInput
strToInput "" = Nothing
strToInput "add" = Just Add
strToInput "substract" = Just Substract
strToInput "multiply" = Just Multiply
strToInput "negate" = Just Negate
strToInput x = if all isDigit (unpack x) then Just (Number (cast x))
                                         else Nothing

-- add the first two elements and put result back into the stack
doAdd : StackCmd () (S (S height)) (S height)
doAdd = do val1 <- Pop
           val2 <- Pop
           Push (val1 + val2)

doSubstract : StackCmd () (S (S height)) (S height)
doSubstract = do val1 <- Pop
                 val2 <- Pop
                 Push (val2 - val1)

doMultiply : StackCmd () (S (S height)) (S height)
doMultiply = do val1 <- Pop
                val2 <- Pop
                Push (val1 * val2)

doNegate : StackCmd () (S height) (S height)
doNegate = do val1 <- Pop
              Push (0 - val1)

doDiscard : StackCmd () (S height) (height)
doDiscard = do res <- Pop
               pure res

mutual 
  tryAdd : StackIO height
  tryAdd {height = S (S h)} = do doAdd
                                 res <- Top
                                 PutStr ("Result: " ++ show res ++ "\n")
                                 stackCalc
                                 
  tryAdd = do PutStr "Fewer than two items on the stack \n"
              stackCalc
  
  trySubstract : StackIO height
  trySubstract {height = S (S h)} = do doSubstract
                                       res <- Top
                                       PutStr ("Result: " ++ show res ++ "\n")
                                       stackCalc
                                 
  trySubstract = do PutStr "Fewer than two items on the stack \n"
                    stackCalc
  
  tryMultiply : StackIO height
  tryMultiply {height = S (S h)} = do doMultiply
                                      res <- Top
                                      PutStr ("Result: " ++ show res ++ "\n")
                                      stackCalc
                                 
  tryMultiply = do PutStr "Fewer than two items on the stack \n"
                   stackCalc
  
  tryNegate : StackIO height
  tryNegate {height = (S h)} = do doNegate
                                  res <- Top
                                  PutStr ("Result: " ++ show res ++ "\n")
                                  stackCalc
                                 
  tryNegate = do PutStr "No items on the stack \n"
                 stackCalc
 
  tryDiscard : StackIO height
  tryDiscard {height = (S h)} = do res <- doDiscard
                                   PutStr ("Result: " ++ show res ++ "\n")
                                   stackCalc
                                 
  tryDiscard = do PutStr "No items on the stack \n"
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
                      Just Substract => trySubstract
                      Just Multiply => tryMultiply
                      Just Negate => tryNegate
                      Just Discard => tryDiscard
  
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
