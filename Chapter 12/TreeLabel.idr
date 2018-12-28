data Tree a = Empty | Node (Tree a) a (Tree a)

testTree : Tree String 
testTree = Node (Node (Node Empty "Jim" Empty) "Fred" (Node Empty "Sheila" Empty)) "Alice" (Node Empty "Bob" (Node Empty "Eve" Empty))
            
flatten : Tree a -> List a
flatten Empty = []
flatten (Node x y z) = (flatten x) ++  y :: (flatten z)

treeLabelWith : Stream labelType -> Tree a -> (Stream labelType, Tree (labelType, a))
treeLabelWith lbls Empty = (lbls, Empty)
treeLabelWith lbls (Node left val right) 
  = let (lblThis :: labelsLeft, labeled_left) = treeLabelWith lbls left
        (lbsbRight, labeled_right) = treeLabelWith labelsLeft right in
        (lbsbRight, Node labeled_left (lblThis,val) labeled_right)

treeLabel : Tree a -> Tree (Integer, a)
treeLabel tree = snd (treeLabelWith [1..] tree)
