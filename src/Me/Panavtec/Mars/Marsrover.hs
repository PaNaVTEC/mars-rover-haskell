module Me.Panavtec.Mars.Marsrover (
  moveMars
  ) where

import           Data.List
import           Data.Maybe

leftDirection = cycle(['N', 'W', 'S', 'E'])
rightDirection = cycle(['N', 'E', 'S', 'W'])

moveMars :: String -> String
moveMars orders = next orders "0,0,N"
  where next [] currentPosition      = currentPosition
        next ('L' : orders) position = next orders (rotate leftDirection position)
        next ('R' : orders) position = next orders (rotate rightDirection position)

rotate :: [Char] -> String -> String
rotate directions (x:',':y:',':dir:_) = x:',':y:',': (next dir) :[]
  where next dir = directions !! nextIndex
        nextIndex = fromJust(elemIndex dir directions) + 1
