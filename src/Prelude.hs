{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home, prune #-}

module Prelude
  ( module All,
    N,
    (!),
    say,
    Sign (..),
    pattern Pos,
    pattern Neg,
    signOf,
    swapF,
    (&&),
    (||),
  )
where

import CoercibleUtils as All
import Control.Arrow as All hiding (first, second)
import Control.Comonad as All
import Control.Comonad.Cofree as All (Cofree)
import Control.Comonad.Trans.Cofree as All (CofreeF (..))
import Control.Monad.Free as All (Free)
import Data.Coerce as All
import Data.Distributive as All
import Data.Functor.Classes as All
import Data.Functor.Compose as All
import Data.Functor.Foldable as All
import Data.Functor.Rep as All
import Data.Profunctor as All
import Data.Profunctor.Strong as All
import Data.Semigroup as All
import Data.These as All
import GHC.Natural (intToNatural)
import NumHask.Prelude as All hiding ((&&), Distributive, First (..), Last (..), fold, yield, (||))

type N = Natural

-- | Positive ("Pos") or negative ("Neg")
newtype Sign
  = Sign Bool
  deriving (Eq, Ord, Show, Read, Generic, Typeable)
  deriving newtype
    ( NFData,
      Enum,
      Bounded,
      BoundedJoinSemiLattice,
      BoundedMeetSemiLattice,
      JoinSemiLattice,
      MeetSemiLattice,
      Multiplicative
    )

pattern Pos = Sign True

pattern Neg = Sign False

{-# COMPLETE Pos, Neg #-}

-- | Use `index` from the `Representable` class for indexing by default
(!) :: forall f a. Representable f => f a -> Rep f -> a
(!) = index

infixl 9 !

(&&) :: MeetSemiLattice a => a -> a -> a
(&&) = (/\)

infixr 3 &&

(||) :: JoinSemiLattice a => a -> a -> a
(||) = (\/)

infixr 2 ||

say :: Text -> IO ()
say = putStrLn

-- | An alternative version of 'sign', allowing the types to differ, but
-- requiring that the resulting type be "Subtractive" and "Multiplicative"
signOf ::
  forall a b.
  (Eq a, Ord a, Additive a) =>
  (Subtractive b, Multiplicative b) =>
  a ->
  b
signOf n
  | n > zero = one
  | n == zero = zero
  | otherwise = negate one

-- | A renaming of "sequence", for situations where it looks nothing like
-- sequencing
--
-- This basically just swaps two wrappers/functors around
swapF :: (Traversable t, Applicative f) => t (f a) -> f (t a)
swapF = sequenceA

instance Signed Sign where
  sign = id
  abs = const Pos

instance (Subtractive a, Multiplicative a) => ToIntegral Sign a where
  toIntegral Pos = one
  toIntegral Neg = negate one

instance Normed Int Natural where
  normL1 = intToNatural . abs
  normL2 = intToNatural . abs

-- NOTE: Because of the way variables are matched and then constrained, Pair Int
-- technically matches the (ExpField a, Normed a a) => Normed (Pair a) a
-- instance, even though Ints don't match the ExpField constraint, so we have to
-- declare this instance as overlapping that one.
instance {-# OVERLAPPING #-} Normed (Pair Int) Int where
  normL1 (Pair x y) = normL1 x + normL1 y
  normL2 (Pair _ _) = throw (NoMethodError "normL2 can't be defined for Int pairs")

instance {-# OVERLAPPING #-} Normed (Pair Int) Natural where
  normL1 (Pair x y) = normL1 x + normL1 y
  normL2 (Pair _ _) = throw (NoMethodError "normL2 can't be defined for Int pairs")

instance {-# OVERLAPPING #-} Normed (Pair Natural) Natural where
  normL1 (Pair x y) = normL1 x + normL1 y
  normL2 (Pair _ _) = throw (NoMethodError "normL2 can't be defined for Int pairs")

-- | So-called taxicab or rectilinear distance for pairs of "Int"s
instance {-# OVERLAPPING #-} Metric (Pair Int) Int where
  distanceL1 a b = normL1 (a - b)
  distanceL2 _ _ = throw (NoMethodError "distanceL2 can't be defined for Int pairs")

instance {-# OVERLAPPING #-} Metric (Pair Int) Natural where
  distanceL1 a b = normL1 (a - b)
  distanceL2 _ _ = throw (NoMethodError "distanceL2 can't be defined for Int pairs")

instance {-# OVERLAPPING #-} Metric (Pair Natural) Natural where
  distanceL1 a b = normL1 (a - b)
  distanceL2 _ _ = throw (NoMethodError "distanceL2 can't be defined for Int pairs")
