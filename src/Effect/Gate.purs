-- | A one-shot latch carrying a value. `wait` suspends until `open` supplies the
-- | value, then every waiter resumes in arrival order; a `wait` after `open`
-- | resumes at once. The background uses one to accept sidebar requests before
-- | boot has finished and serve each of them exactly once when it has.
module Effect.Gate
  ( Gate
  , new
  , open
  , wait
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Ref as Ref

-- | Waiters while closed, the value once open.
newtype Gate a = Gate (Ref.Ref (Either (Array (a -> Effect Unit)) a))

new :: forall a. Effect (Gate a)
new = Gate <$> Ref.new (Left [])

-- | Opening twice keeps the first value.
open :: forall a. Gate a -> a -> Effect Unit
open (Gate ref) a = Ref.read ref >>= case _ of
  Left waiters -> do
    Ref.write (Right a) ref
    traverse_ (_ $ a) waiters
  Right _ -> pure unit

wait :: forall a. Gate a -> Aff a
wait (Gate ref) = makeAff \k -> do
  Ref.read ref >>= case _ of
    Right a -> k (Right a)
    Left waiters -> Ref.write (Left (Array.snoc waiters (k <<< Right))) ref
  pure nonCanceler
