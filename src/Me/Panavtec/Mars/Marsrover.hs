module Me.Panavtec.Mars.Marsrover (
  moveMars
  ) where

moveMars :: String -> String
moveMars orders = next orders "0,0,N"
  where next [] currentPosition     = currentPosition
        next ('L' : orders) (x : ',' : y : ',' : 'N' : _) =  next orders "0,0,W"
        next ('L' : orders) (x : ',' : y : ',' : 'W' : _)=  next orders "0,0,S"
        next ('L' : orders) (x : ',' : y : ',' : 'S' : _)=  next orders "0,0,E"
