
import GameState
import Test.Hspec
import Data.Sequence (fromList)

opositeMovementSpec :: Spec
opositeMovementSpec = do
    describe "oppositeMovement" $ do
      it "returs South when given North" $ do
        opositeMovement North `shouldBe` South
      it "returs North when given South" $ do
        opositeMovement South `shouldBe` North
      it "returs West when given East" $ do
        opositeMovement East `shouldBe` West
      it "returs East when given West" $ do
        opositeMovement West `shouldBe` East

inSnakeSpec :: Spec
inSnakeSpec = do
  describe "Check if a point is in the snake" $ do
    let snake_seq = SnakeSeq (1,1) (fromList [(1,2), (1,3)])
    it "returns True if the point is in the snake" $ do
      inSnake (1,1) snake_seq `shouldBe` True
    it "returns True if the point is in the snake" $ do
      inSnake (1,2) snake_seq `shouldBe` True
    it "returns False if the point is not in the snake" $ do
      inSnake (1,4) snake_seq `shouldBe` False


main :: IO ()
main = hspec $ do
    opositeMovementSpec
    inSnakeSpec
