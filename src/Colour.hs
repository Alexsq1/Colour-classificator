-- | Module for colour representation

module Colour (Colour(..), Point(..), newPoint, plot) where

type Point = (Int, Int, Int)

-- | Datatype for Colours
data Colour = Red | Blue | LightGreen | DarkGreen | Yellow | Purple | Pink | Brown | LightBlue | Orange | White | Black | Grey deriving (Show, Eq, Enum, Bounded)

-- | Creates a point (checks that numbers are in range)
newPoint :: Int -> Int -> Int -> Point
newPoint x y z = (check x, check y, check z)
    where
	check :: Int -> Int
	check x
	    | x >= 0 && x <= 255 = x
	    | otherwise = error "Coordinates must be between 0 and 255"

-- | Returns the RGB values of a colour 
plot :: Colour -> Point

plot Red = newPoint 255 0 0
plot LightGreen = newPoint 0 255 0
plot Blue = newPoint 0 0 255

plot Yellow = newPoint 255 255 0
plot Purple = newPoint 177 0 255
plot LightBlue = newPoint 0 255 255

plot DarkGreen = newPoint 0 177 0

plot Orange = newPoint 255 177 0

plot White = newPoint 255 255 255
plot Black = newPoint 0 0 0
plot Grey = newPoint 177 177 177

plot Pink = newPoint 255 63 191
plot Brown = newPoint 100 50 40
