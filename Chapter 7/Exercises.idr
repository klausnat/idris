data Shape = Triangle Double Double 
           | Rectangle Double Double
           | Circle Double

Eq Shape where                      
  (==) (Triangle x z) (Triangle y w) = case x == y && z == w of
                                            True => True
                                            False => False
  (==) (Rectangle x z) (Rectangle y w) = case x == y && z == w of
                                              False => False 
                                              True => True
  (==) (Circle x) (Circle y) = case x == y of
                                    False => False 
                                    True => True
  (==) _ _ = False                                    

Ord Shape where
  compare x y = compare (calcArea x) (calcArea y) where
                        calcArea : Shape -> Double
                        calcArea (Triangle x z) = x * z / 2
                        calcArea (Rectangle x z) = x * z 
                        calcArea (Circle x) = pi * x * x / 2

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]
