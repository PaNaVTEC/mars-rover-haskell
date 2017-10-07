module Me.Panavtec.Mars.Marsrover (
  moveMars
  ) where

moveMars :: String -> String
moveMars orders = next orders "0,0,N"
  where next [] currentPosition           = currentPosition
        next ('L' : restOfOrders) "0,0,N" =  next restOfOrders "0,0,W"
        next ('L' : restOfOrders) "0,0,W" =  next restOfOrders "0,0,S"
