module Me.Panavtec.Mars.Marsrover (
  moveMars
  ) where

import           Data.Char
import           Data.List
import           Data.Maybe

bounds = cycle([0..9])
leftDirection = cycle(['N', 'W', 'S', 'E'])
rightDirection = cycle(['N', 'E', 'S', 'W'])
initialPosition = "0,0,N"

moveMars :: String -> String
moveMars orders = foldl (\position order -> if order == 'M' then move position else rotate order position) initialPosition orders

makePosition :: Char -> Char -> Char -> String
makePosition x y d = x : ',' : y : ',' : d : []

move :: String -> String
move (x:',':y:',':direction:_)
  | direction == 'E' = makePosition (increment x) y direction
  | direction == 'N' = makePosition x (increment y) direction
  | direction == 'W' = makePosition (decrement x) y direction
  | direction == 'S' = makePosition x (decrement y) direction
  where
    increment bound = intToDigit $ bounds !! (digitToInt bound + 1)
    checkNegative bound = if bound == '0' then 9 else digitToInt bound - 1
    decrement bound = intToDigit $ bounds !! checkNegative bound

rotate :: Char -> String -> String
rotate direction (x:',':y:',':currentDirection:_) = makePosition x y nextDirection
  where directions = if direction == 'L' then leftDirection else rightDirection
        nextDirection = directions !! nextIndex
        currentIndex = elemIndex currentDirection directions
        nextIndex = fromJust(currentIndex) + 1
