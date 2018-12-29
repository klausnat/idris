data Tree a = Empty | Node (Tree a) a (Tree a)

testTree : Tree String 
testTree = Node (Node (Node Empty "Jim" Empty) "Fred" (Node Empty "Sheila" Empty)) "Alice" (Node Empty "Bob" (Node Empty "Eve" Empty))
            
flatten : Tree a -> List a
flatten Empty = []
flatten (Node x y z) = (flatten x) ++  y :: (flatten z)

data State : (stateType : Type) -> Type -> Type where
     Get : State stateType stateType
     Put : stateType -> State stateType ()
     
     Pure : ty -> State stateType ty     
     Bind : State stateType a -> (a -> State stateType b) -> State stateType b
     
get : State stateType stateType
get = Get

put : stateType -> State stateType ()
put = Put

pure : ty -> State stateType ty
pure = Pure

(>>=) : State stateType a -> (a -> State stateType b) -> State stateType b
(>>=) = Bind

treeLabelWith : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWith Empty = Pure Empty
treeLabelWith (Node left val right) = do left_labeled <- treeLabelWith left
                                         (labelVal :: rest) <- get
                                         put rest
                                         right_labeled <- treeLabelWith right
                                         pure (Node left_labeled (labelVal, val) right_labeled)

runState : State stateType a -> (st : stateType) -> (a, stateType)
runState Get st = (st, st)
runState (Put newState) st = ((),newState)
runState (Pure x) st = (x,st)
runState (Bind cmd prog) st = let (val, nextState) = runState cmd st in
                                        runState (prog val) nextState
                                        
treeLabel : Tree a -> Tree (Integer, a)
treeLabel tree = fst (runState (treeLabelWith tree) [1..])

