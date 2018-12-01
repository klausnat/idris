record Album where
  constructor MkAlbum 
  title : String
  artist : String
  year : Integer

Eq Album where 
  (==) (MkAlbum title artist year) (MkAlbum title' artist' year') = title == title' && artist == artist' && year == year' 

Ord Album where 
    compare (MkAlbum title artist year) (MkAlbum title' artist' year') 
         = case compare artist artist' of
                 EQ => case compare year year' of
                            EQ => compare title title'
                            diff_years => diff_years
                 diff_art => diff_art
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
