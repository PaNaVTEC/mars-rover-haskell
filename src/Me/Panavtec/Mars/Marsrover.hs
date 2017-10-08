module Me.Panavtec.Mars.Marsrover (
  moveMars
  ) where

import           Data.List
import           Data.Maybe

leftDirection = cycle(['N', 'W', 'S', 'E'])
rightDirection = cycle(['N', 'E', 'S', 'W'])

moveMars :: String -> String
moveMars orders = foldl (\b a -> rotateTo a b) "0,0,N" orders
  where rotateTo 'L' currentPos = rotate leftDirection currentPos
        rotateTo 'R' currentPos = rotate rightDirection currentPos

rotate :: [Char] -> String -> String
rotate directions (x:',':y:',':currentDirection:_) = x:',':y:',': nextDirection :[]
  where nextDirection = directions !! nextIndex
        currentIndex = elemIndex currentDirection directions
        nextIndex = fromJust(currentIndex) + 1
