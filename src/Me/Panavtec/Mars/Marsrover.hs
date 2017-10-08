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

createPosition :: Char -> Char -> Char -> String
createPosition x y d = x : ',' : y : ',' : d : []

move :: String -> String
move (x:',':y:',':direction:_)
  | direction == 'E' = createPosition (increment x) y direction
  | direction == 'N' = createPosition x (increment y) direction
  | direction == 'W' = createPosition (decrement x) y direction
  | direction == 'S' = createPosition x (decrement y) direction
  where
    bounds = cycle([0..9])
    increment bound = intToDigit $ bounds !! (digitToInt bound + 1)
    checkNegative bound = if bound == '0' then 9 else digitToInt bound - 1
    decrement bound = intToDigit $ bounds !! checkNegative bound

rotate :: Char -> String -> String
rotate direction (x:',':y:',':currentDirection:_) = createPosition x y nextDirection
  where directions = if direction == 'L' then leftDirection else rightDirection
        nextDirection = directions !! nextIndex
        currentIndex = elemIndex currentDirection directions
        nextIndex = fromJust(currentIndex) + 1
