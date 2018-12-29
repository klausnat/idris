import Control.Monad.State

-- 1. Write a function that updates a state by applying a function to the current state

update : (stateType -> stateType) -> State stateType ()
update f = do cur <- get
              put (f cur)

increase : Nat -> State Nat ()
increase x = update (+x)

-- 2. Write a function that uses State to count the number of occurrences of Empty in a tree

data Tree a = Empty | Node (Tree a) a (Tree a)

testTree : Tree String 
testTree = Node (Node (Node Empty "Jim" Empty) "Fred" (Node Empty "Sheila" Empty)) "Alice" (Node Empty "Bob" (Node Empty "Eve" Empty))
            
flatten : Tree a -> List a
flatten Empty = []
flatten (Node x y z) = (flatten x) ++  y :: (flatten z)

countEmpty : Tree a -> State Nat ()
countEmpty Empty = increase 1
countEmpty (Node left val right) = do countEmpty left
                                      countEmpty right
                                      

-- 3. Write a function that counts the number of occurrences of both Empty and Node
countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = do (e, n) <- get
                          put (e + 1, n)
                          
countEmptyNode (Node left val right) = do countEmptyNode left
                                          countEmptyNode right
                                          (e, n) <- get
                                          put (e, n + 1)
                                          


