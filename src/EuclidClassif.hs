-- | Colour classifier by euclidean distance
module EuclidClassif (classifier) where
import Colour

-- | Datatype to represent distance to each colour
data DistP = DP(Color, Int) deriving (Eq, Show)

instance Ord DistP where
    compare (DP t1) (DP t2) = compare (snd t1) (snd t2)

distance :: Point -> Point -> Int
distance (C x1, C y1, C z1) (C x2, C y2, C z2) = (x1 - x2) ^ 2 + (y2 - y1) ^ 2 + (z1 - z2) ^ 2

allColors :: [Color]
allColors = [Red .. ]

list_coords :: [(Color, Point)]
list_coords = zip allColors (map coord allColors)

list_dist :: Point -> [DistP]
list_dist orig = map (\(c, p) -> DP(c, distance orig p) ) list_coords

map_fst :: (a -> c) -> [(a, b)] -> [(c, b)]
map_fst f = map (\(x, y) -> (f x, y))

map_snd :: (b -> c) -> [(a, b)] -> [(a, c)]
map_snd f = map (\(x, y) -> (x, f y))

-- unnecesary ?
unwrap :: DistP -> (Color, Int)
unwrap (DP t) = t

classifier :: Point -> Color
classifier p = fst (unwrap (minimum (list_dist p)))

count :: Eq a => a -> [a] -> Int
count a = length . filter (==a)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups(filter (/=x) xs)

{- previously used for testing
todos :: [Color]
todos = [clasificador (newPoint x y z) | x<-l, y<-l, z<-l]
    where l = 0:[5,10..255]

porcentajes :: [(Color, Int)]  
porcentajes = [(x, count x todos) | x <- allColors]
-}
--Fallos raros (52, 179, 166), (62, 8, 133)
