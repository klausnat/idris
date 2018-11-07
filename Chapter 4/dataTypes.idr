{- Enumerated types
   Union types
   Recursive types
   Generic types
   Dependent types
-}

||| Exercise 6 : Write a function biggestTriangle that returns the area of the biggest triangle in a picture, or Nothing if there are no triangles

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double
           
area : Shape -> Double           
area (Triangle x y) = 0.5 * x * y
area (Rectangle x y) = x * y
area (Circle x) = pi * x * x

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2             
                                       
pictureArea : Picture -> Double             
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

data Biggest = NoTriangle | Size Double

biggestTriangle : Picture -> Biggest
biggestTriangle (Primitive (Triangle x y)) = Size (area (Triangle x y))
biggestTriangle (Primitive (Rectangle x y)) = NoTriangle
biggestTriangle (Primitive (Circle x)) = NoTriangle
biggestTriangle (Combine pic pic1) = case biggestTriangle pic of
                                          NoTriangle => biggestTriangle pic1
                                          Size a     => case biggestTriangle pic1 of
                                                             NoTriangle => Size a
                                                             Size b     => if a >= b then Size a else Size b                     
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

