module Me.Panavtec.Mars.Marsrover (
  moveMars
  ) where

import           Data.Char
import           Data.List
import           Data.Maybe

leftDirection = cycle(['N', 'W', 'S', 'E'])
rightDirection = cycle(['N', 'E', 'S', 'W'])
initialPosition = "0,0,N"

moveMars :: String -> String
moveMars orders = foldl (\position order -> if order == 'M' then move position else rotate order position) initialPosition orders

move :: String -> String
move (x:',':y:',':direction:_)
  | direction == 'E' = moveEast:',':y:',':'E':[]
  | direction == 'N' = x:',':moveNorth:',':'N':[]
  where
    bounds = cycle([0..9])
    moveNorth = intToDigit $ bounds !! (digitToInt y + 1)
    moveEast = intToDigit $ bounds !! (digitToInt x + 1)

rotate :: Char -> String -> String
rotate direction (x:',':y:',':currentDirection:_) = x:',':y:',': nextDirection :[]
  where directions = if direction == 'L' then leftDirection else rightDirection
        nextDirection = directions !! nextIndex
        currentIndex = elemIndex currentDirection directions
        nextIndex = fromJust(currentIndex) + 1
