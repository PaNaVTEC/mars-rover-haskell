module Me.Panavtec.Mars.Marsrover (
  moveMars
  ) where

import           Data.List
import           Data.Maybe

leftDirection = cycle(['N', 'W', 'S', 'E'])
rightDirection = cycle(['N', 'E', 'S', 'W'])

moveMars :: String -> String
moveMars orders = foldl (\b a -> rotate a b) "0,0,N" orders

rotate :: Char -> String -> String
rotate direction (x:',':y:',':currentDirection:_) = x:',':y:',': nextDirection :[]
  where directions = if direction == 'L' then leftDirection else rightDirection
        nextDirection = directions !! nextIndex
        currentIndex = elemIndex currentDirection directions
        nextIndex = fromJust(currentIndex) + 1
