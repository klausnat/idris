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

||| 3. Define a recursive data type Expr for integer arifmetic expression

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

%name Expr expr, expr1, expr2
                    
||| 4. Write a function evaluate that evaluates an integer arifmetic expression

evaluate : Expr -> Int                    
evaluate (Val x) = x
evaluate (Add expr expr1) = evaluate expr + evaluate expr1
evaluate (Sub expr expr1) = evaluate expr - evaluate expr1
evaluate (Mult expr expr1) = evaluate expr * evaluate expr1

||| 5. Write a function maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a that returns a larger of the two inputs, or Nothing if both inputs are Nothing

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just x) = Just x 
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = case compare x y of
                             LT => Just y
                             EQ => Just x
                             GT => Just x


{- 6. biggestTriangle function is in the file (Chapter 4/dataTypes.idr) -}
