record Album where
  constructor MkAlbum 
  title : String
  artist : String
  year : Integer

Eq Album where 
   
Ord Album where
  compare x y = ?Ord_rhs_1
  (<) x y = ?Ord_rhs_2
  (>) x y = ?Ord_rhs_3
  (<=) x y = ?Ord_rhs_4
  (>=) x y = ?Ord_rhs_5
  max x y = ?Ord_rhs_6
  min x y = ?Ord_rhs_7

clouds : Album
clouds = MkAlbum "Hello" "Iris Martin" 1983

trees : Album 
trees = MkAlbum "Big Trees" "Mary Smith" 8976

grass : Album
grass = MkAlbum "Green Gras" "Anna Maria" 8372

sky : Album
sky = MkAlbum "Blue Sky" "Jonny Depp" 1234

collection : List Album
collection = [clouds, trees, grass, sky]
