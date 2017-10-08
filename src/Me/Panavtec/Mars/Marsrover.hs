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
rotateRight (x:',':y:',':'N':_) =  x:',':y:',':'E':[]
rotateRight (x:',':y:',':'W':_) =  x:',':y:',':'N':[]
rotateRight (x:',':y:',':'S':_) =  x:',':y:',':'W':[]
rotateRight (x:',':y:',':'E':_) =  x:',':y:',':'S':[]

rotateLeft :: String -> String
rotateLeft (x:',':y:',':dir:_) = x:',':y:',': (next dir) :[]
  where next dir = leftDirection !! nextIndex
        nextIndex = fromJust(elemIndex dir leftDirection) + 1
--rotateLeft (x:',':y:',':'N':_) = x:',':y:',':'W':[]
--rotateLeft (x:',':y:',':'W':_) = x:',':y:',':'S':[]
--rotateLeft (x:',':y:',':'S':_) = x:',':y:',':'E':[]
--rotateLeft (x:',':y:',':'E':_) = x:',':y:',':'N':[]
