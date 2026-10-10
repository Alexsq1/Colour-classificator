module Main (main) where

import Colour(newPoint)
import EuclidClassif(classifier)

main :: IO ()
main = do 
	putStrLn "Insert a colour in RGB (3 coordinates separated by spaces), or empty for exit"
	l <- getLine
	case (words l) of
		[] -> pure ()
		[_,_,_] -> do 
				let [r,g,b] = map read (words l) :: [Int]
				let p = newPoint r g b
				putStrLn $ "Point " ++ show p ++ " seems to be " ++ show (classifier p) ++ "\n"
				main
		_ -> do 
			putStrLn "Incorrect format\n"
			main
