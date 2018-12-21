-- exercise 2. Define a view that allows other modules to inspect the abstract Shape data type.
-- in file Shape.idr this view was defined. and here it was imported

import Shape

area : Shape -> Double
area s with (shapeView s)
  area (triangle base height) | STriangle = 0.5 * base * height
  area (rectangle lenght height) | SRectangle = lenght * height
  area (circle radius) | SCircle = pi * radius * radius


