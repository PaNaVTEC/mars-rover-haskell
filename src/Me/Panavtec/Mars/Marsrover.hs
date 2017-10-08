module Me.Panavtec.Mars.Marsrover (
  moveMars
  ) where

import           Data.Char
import           Data.List
import           Data.Maybe

boardBound :: Int
boardBound = 9

bounds :: [Int]
bounds = cycle([0..boardBound])

leftDirection :: [Char]
leftDirection = cycle(['N', 'W', 'S', 'E'])

rightDirection :: [Char]
rightDirection = cycle(['N', 'E', 'S', 'W'])

initialPosition :: String
initialPosition = "0,0,N"

moveMars :: String -> String
moveMars = foldl interpret initialPosition
  where interpret position order
          | order == 'M' = move position
          | order == 'R' || order == 'L'  = rotate order position

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
    decrement bound = intToDigit $ bounds !! checkNegative bound
    checkNegative bound
      | bound == '0' = boardBound
      | otherwise = digitToInt bound - 1

rotate :: Char -> String -> String
rotate direction (x:',':y:',':currentDirection:_) = makePosition x y nextDirection
  where directions = if direction == 'L' then leftDirection else rightDirection
        nextDirection = directions !! nextIndex
        currentIndex = elemIndex currentDirection directions
        nextIndex = fromJust(currentIndex) + 1
