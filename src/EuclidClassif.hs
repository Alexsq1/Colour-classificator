-- | Colour classifier by euclidean distance
module EuclidClassif (classifier) where
import Colour

-- | Datatype to represent distance to each colour
data DistP = DP(Colour, Int) deriving (Eq, Show)

-- | Order of DistP (simplifies the minimum distance code)
instance Ord DistP where
    compare (DP t1) (DP t2) = compare (snd t1) (snd t2)

-- | Euclidean distance
distance :: Point -> Point -> Int
distance (x1, y1, z1) (x2, y2, z2) = (x1 - x2) ^ t + (y2 - y1) ^ t + (z1 - z2) ^ t
	where
		t = 2 :: Int

-- | Classifies a point
classifier :: Point -> Colour
classifier p = fst ( (\(DP t) -> t) (minimum (list_dist p)))

-- | List that contains all colours
allColours :: [Colour]
allColours = [Red .. ]

-- | List that contains all colours with their coordinates
list_coords :: [(Colour, Point)]
list_coords = zip allColours (map plot allColours)

-- | List that contains all colours with the distance to a given
list_dist :: Point -> [DistP]
list_dist orig = map (\(c, p) -> DP(c, distance orig p) ) list_coords

--Strange failures: (52, 179, 166), (62, 8, 133)
