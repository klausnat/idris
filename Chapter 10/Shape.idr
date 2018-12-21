module Shape

-- Exercise 2. Define a View that allows other modules to inspect the abstract Shape data type

public export
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double  

-- functions for building Shape
export 
triangle : (base : Double) -> (height : Double) -> Shape
triangle = Triangle

export
rectangle : (lenght : Double) -> (height : Double) -> Shape
rectangle = Rectangle

export
circle : (radius : Double) -> Shape
circle = Circle

-- inspecting the Shape data type with a view

public export
data ShapeView : Shape -> Type where
     STriangle : ShapeView (triangle base height)
     SRectangle : ShapeView (rectangle lenght height)
     SCircle : ShapeView (circle radius)

export 
shapeView : (shape : Shape) -> ShapeView shape
shapeView (Triangle x y) = STriangle
shapeView (Rectangle x y) = SRectangle
shapeView (Circle x) = SCircle

{-
-- Functions for accessing the store

export
empty : DataStore schema
empty = MkData 0 []

export 
addToStore : (value : SchemaType schema) -> (store : DataStore schema) -> DataStore schema
addToStore value (MkData _ items) = MkData _ (value :: items)

-- Traversing the store's contents with a view
public export
data StoreView : DataStore schema -> Type where
     SNil : StoreView empty
     SAdd : (rec : StoreView store) -> StoreView (addToStore value store)

storeViewHelp : (items : Vect size (SchemaType schema)) -> StoreView (MkData size items)
storeViewHelp [] = SNil
storeViewHelp (val :: xs) = SAdd (storeViewHelp xs)

export
storeView : (store : DataStore schema) -> StoreView store
storeView (MkData size items) = storeViewHelp items
-}
