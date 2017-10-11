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

data Coordinate = Coordinate Int Int
data Direction = N | W | S | E deriving (Show, Eq)
data Position = Position Coordinate Direction
data Rotate = L | R deriving Eq

leftDirection :: [Direction]
leftDirection = cycle([N, W, S, E])

rightDirection :: [Direction]
rightDirection = cycle([N, E, S, W])

initialPosition :: Position
initialPosition = Position (Coordinate 0 0) N

moveMars :: String -> String
moveMars = showPosition . foldl interpret initialPosition
  where interpret position order
          | order == 'M' = move position
          | order == 'L' = rotateLeft position
          | order == 'R' = rotateRight position

showPosition :: Position -> String
showPosition (Position (Coordinate x y) d) = xx : ',' : yy : ',' : dd : []
  where xx = intToDigit x
        yy = intToDigit y
        dd = head $ show d

move :: Position -> Position
move (Position (Coordinate x y) direction)
  | direction == E = Position (Coordinate (increment x) y) direction
  | direction == N = Position (Coordinate x (increment y)) direction
  | direction == W = Position (Coordinate (decrement x) y) direction
  | direction == S = Position (Coordinate x (decrement y)) direction
  where
    increment bound = modify bound (+1)
    decrement bound = modify bound (subtract 1)
    modify bound f = bounds !! noNegatives (f bound)
    noNegatives a
      | a < 0 = boardBound
      | otherwise = a

rotateLeft :: Position -> Position
rotateLeft = rotate L

rotateRight :: Position -> Position
rotateRight = rotate R

rotate :: Rotate -> Position -> Position
rotate direction (Position (Coordinate x y) currentDirection) = Position (Coordinate x y) nextDirection
  where directions
          | direction == L = leftDirection
          | direction == R = rightDirection
        nextDirection = directions !! nextIndex
        currentIndex = elemIndex currentDirection directions
        nextIndex = fromJust(currentIndex) + 1
