import Control.Monad.State

{-
State : (stateType : Type) -> (ty : Type) -> Type
runState : State stateType a -> stateType -> (a, stateType)
evalState : State stateType a -> stateType -> a
execState : State stateType -> stateType -> stateType 

get : State stateType stateType
put : stateType -> State stateType () 

(>>=) : State stateType a -> (a -> State stateType b) -> State stateType b
-}

-- increase a state by a given value

increase : Nat -> State Nat ()
increase inc = do current <- get
                  put (current + inc)

data Tree a = Empty | Node (Tree a) a (Tree a)

testTree : Tree String 
testTree = Node (Node (Node Empty "Jim" Empty) "Fred" (Node Empty "Sheila" Empty)) "Alice" (Node Empty "Bob" (Node Empty "Eve" Empty))
            
flatten : Tree a -> List a
flatten Empty = []
flatten (Node x y z) = (flatten x) ++  y :: (flatten z)

treeLabelWith : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWith Empty = pure Empty
treeLabelWith (Node leftTree val rightTree) 
         = do leftTreeLabeled <- treeLabelWith leftTree
              (cur_val :: rest) <- get
              put rest
              rightTreeLabeled <- treeLabelWith rightTree
              pure (Node leftTreeLabeled (cur_val, val) rightTreeLabeled)

treeLabel : Tree a -> Tree (Integer, a)
treeLabel tree = evalState (treeLabelWith tree) [1..]
