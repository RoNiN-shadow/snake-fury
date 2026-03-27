
import GameState
import Test.Hspec

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

main :: IO ()
main = hspec $ do
    opositeMovementSpec
      
