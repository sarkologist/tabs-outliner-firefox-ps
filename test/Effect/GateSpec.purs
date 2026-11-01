module Test.Effect.GateSpec where

import Prelude

import Data.Array as Array
import Effect.Aff (forkAff, joinFiber)
import Effect.Class (liftEffect)
import Effect.Gate as Gate
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Effect.Gate" do
  it "holds waiters until opened, then resumes each once, in arrival order" do
    gate <- liftEffect Gate.new
    log <- liftEffect (Ref.new [])
    let waiter tag = forkAff do
          v <- Gate.wait gate
          liftEffect (Ref.modify_ (_ `Array.snoc` (tag <> v)) log)
    a <- waiter "a:"
    b <- waiter "b:"
    held <- liftEffect (Ref.read log)
    held `shouldEqual` []
    liftEffect (Gate.open gate "x")
    joinFiber a
    joinFiber b
    served <- liftEffect (Ref.read log)
    served `shouldEqual` [ "a:x", "b:x" ]

  it "resumes a wait made after opening at once" do
    gate <- liftEffect Gate.new
    liftEffect (Gate.open gate 1)
    v <- Gate.wait gate
    v `shouldEqual` 1

  it "keeps the first value when opened twice, without re-running waiters" do
    gate <- liftEffect Gate.new
    runs <- liftEffect (Ref.new 0)
    f <- forkAff do
      _ <- Gate.wait gate
      liftEffect (Ref.modify_ (_ + 1) runs)
    liftEffect do
      Gate.open gate 1
      Gate.open gate 2
    joinFiber f
    v <- Gate.wait gate
    v `shouldEqual` 1
    n <- liftEffect (Ref.read runs)
    n `shouldEqual` 1
