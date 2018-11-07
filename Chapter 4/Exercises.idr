||| 1. Write a function listToTree : Ord a => List a -> Tree a, that inserts every element of a list into a binary search tree

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)
               
%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem                              
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => orig
                                      GT => Node left val (insert x right)

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

||| 2. Write a function treeToList : Tree a -> List a 
treeToList : Tree a -> List a 
treeToList Empty = []
treeToList (Node tree x tree1) = treeToList tree ++ [x] ++ treeToList tree1
