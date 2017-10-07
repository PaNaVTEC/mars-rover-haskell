module Me.Panavtec.Mars.Marsrover (
  moveMars
  ) where

moveMars :: String -> String
moveMars orders = next orders "0,0,N"
  where next [] currentPosition      = currentPosition
        next ('L' : orders) position =  next orders (rotateLeft position)
        next ('R' : orders) position =  next orders (rotateRight position)

rotateRight :: String -> String
rotateRight (x:',':y:',':'N':_) =  x:',':y:',':'E':[]

rotateLeft :: String -> String
rotateLeft (x:',':y:',':'N':_) =  x:',':y:',':'W':[]
rotateLeft (x:',':y:',':'W':_) =  x:',':y:',':'S':[]
rotateLeft (x:',':y:',':'S':_) =  x:',':y:',':'E':[]
rotateLeft (x:',':y:',':'E':_) =  x:',':y:',':'N':[]
