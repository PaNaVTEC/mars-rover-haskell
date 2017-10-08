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
        next ('L' : orders) position = next orders (rotateLeft position)
        next ('R' : orders) position = next orders (rotateRight position)

rotateRight :: String -> String
rotateRight (x:',':y:',':dir:_) = x:',':y:',': (next dir) :[]
  where next dir = rightDirection !! nextIndex
        nextIndex = fromJust(elemIndex dir rightDirection) + 1

rotateLeft :: String -> String
rotateLeft (x:',':y:',':dir:_) = x:',':y:',': (next dir) :[]
  where next dir = leftDirection !! nextIndex
        nextIndex = fromJust(elemIndex dir leftDirection) + 1
