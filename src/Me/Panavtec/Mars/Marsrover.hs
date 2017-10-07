module Me.Panavtec.Mars.Marsrover (
  moveMars
  ) where

moveMars :: String -> String
moveMars orders = next orders "0,0,N"
  where next [] currentPosition      = currentPosition
        next ('L' : orders) position =  next orders (rotateLeft position)

rotateLeft :: String -> String
rotateLeft (x : ',' : y : ',' : 'N' : _) =  "0,0,W"
rotateLeft (x : ',' : y : ',' : 'W' : _) =  "0,0,S"
rotateLeft (x : ',' : y : ',' : 'S' : _) =  "0,0,E"
rotateLeft (x : ',' : y : ',' : 'E' : _) =  "0,0,N"
