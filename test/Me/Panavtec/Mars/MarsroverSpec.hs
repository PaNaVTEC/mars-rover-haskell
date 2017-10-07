module Me.Panavtec.Mars.MarsroverSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Me.Panavtec.Mars.Marsrover

-- Develop an api that moves a rover around on a grid.
--
-- Rules:
--
-- You are given the initial starting point (0,0,N) of a rover.
-- 0,0 are X,Y co-ordinates on a grid of (10,10).
-- N is the direction it is facing (i.e. N,S,E,W).
-- L and R allow the rover to rotate left and right.
-- M allows the rover to move one point in the current direction.
-- The rover receives a char array of commands e.g. RMMLM and returns the finishing point after the moves e.g. 2,1,N
-- The rover wraps around if it reaches the end of the grid.
-- The grid may have obstacles. If a given sequence of commands encounters an obstacle, the rover moves up to the last possible point and reports the obstacle e.g. O,2,2,N

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Rotations" $ do
    it "1 Left" $ do
      moveMars "L" `shouldBe` "0,0,W"
    it "2 Left" $ do
      moveMars "LL" `shouldBe` "0,0,S"
    it "3 Left" $ do
      moveMars "LLL" `shouldBe` "0,0,E"
    it "4 Left" $ do
      moveMars "LLLL" `shouldBe` "0,0,N"
