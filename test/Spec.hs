
import GameState
import Test.Hspec
import Data.Sequence (fromList)
import System.Random (mkStdGen)
import RenderState (BoardInfo(BoardInfo))

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

nextHeadSpec :: Spec
nextHeadSpec = do
  describe "Calculate de new of the snake" $ do
    let snake_seq = SnakeSeq (1,1) (fromList [(1,2), (1,3)])
    let apple_pos = (2,2)
    let board_info = BoardInfo 4 4
    let game_state1 = GameState snake_seq apple_pos West $ mkStdGen 1
    let game_state2 = GameState snake_seq apple_pos South $ mkStdGen 1
    let game_state3 = GameState snake_seq apple_pos North $ mkStdGen 1
    it "check for West" $ do
      nextHead board_info game_state1 `shouldBe` (1,4)
    it "check for South" $ do
      nextHead board_info game_state2 `shouldBe` (2,1)
    it "check for North" $ do
      nextHead board_info game_state3 `shouldBe` (4,1)

main :: IO ()
main = hspec $ do
    opositeMovementSpec
    inSnakeSpec
    nextHeadSpec
