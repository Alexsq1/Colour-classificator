-- | Module for colour representation

module Colour (Color(..), Point(..), Coord(..), newPoint, coord) where

type Point = (Coord, Coord, Coord)
data Color = Red | Blue | LightGreen | DarkGreen | Yellow | Purple | Pink | Brown | LightBlue | Orange | White | Black | Grey deriving (Show, Eq, Enum, Bounded)
data Coord = C Int deriving (Eq, Ord)
--data DistP = DP(Color, Int) deriving (Eq, Show)

instance Bounded Coord where
    minBound = C 0
    maxBound = C 255
instance Show Coord where
    show (C x) = show x

newPoint :: Int -> Int -> Int -> Point
newPoint x y z = (newCoord x, newCoord y, newCoord z)

newCoord :: Int -> Coord
newCoord x
    | valid x = C x
    | otherwise = error "Coordinates must be between 0 and 255"

valid :: Int -> Bool
valid x = x >= 0 && x <= 255

coord :: Color -> Point

coord Red = newPoint 255 0 0
coord LightGreen = newPoint 0 255 0
coord Blue = newPoint 0 0 255

coord Yellow = newPoint 255 255 0
coord Purple = newPoint 177 0 255
coord LightBlue = newPoint 0 255 255

coord DarkGreen = newPoint 0 177 0

coord Orange = newPoint 255 177 0

coord White = newPoint 255 255 255
coord Black = newPoint 0 0 0
coord Grey = newPoint 177 177 177

coord Pink = newPoint 255 63 191
coord Brown = newPoint 100 50 40


----Fallos raros (52, 179, 166), (62, 8, 133)
