{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
module FreeBool (BoolAlg(..)) where

import Data.Data (Data)

class BoolAlg a where
  (&&&) :: a -> a -> a
  (|||) :: a -> a -> a
  bnot :: a -> a
  bfalse :: a
  btrue :: a

instance BoolAlg b => BoolAlg (a -> b) where
  (&&&) f g x = f x &&& g x
  (|||) f g x = f x ||| g x
  bnot f = bnot . f
  bfalse = const bfalse
  btrue = const btrue

instance BoolAlg Bool where
  (&&&) = (&&)
  (|||) = (||)
  bnot = not
  bfalse = False
  btrue = True

fromBool :: Bool -> FreeBool a
fromBool True  = BTrue
fromBool False = BFalse

data FreeBool a = BAnd (FreeBool a) (FreeBool a)
                | BOr (FreeBool a) (FreeBool a)
                | BNot (FreeBool a)
                | BFalse
                | BTrue
                | Pure a
                deriving (Show,Eq,Functor,Data)

instance Foldable FreeBool where
  foldMap f (BAnd lhs rhs) = foldMap f lhs <> foldMap f rhs
  foldMap f (BOr lhs rhs) = foldMap f lhs <> foldMap f rhs
  foldMap f (BNot fb) = foldMap f fb
  foldMap _ BFalse = mempty
  foldMap _ BTrue = mempty
  foldMap f (Pure x) = f x

instance Traversable FreeBool where
  traverse f (BAnd lhs rhs) = BAnd <$> traverse f lhs <*> traverse f rhs
  traverse f (BOr lhs rhs) = BOr <$> traverse f lhs <*> traverse f rhs
  traverse f (BNot fb) = BNot <$> traverse f fb
  traverse _ BFalse = pure BFalse
  traverse _ BTrue = pure BTrue
  traverse f (Pure x) = Pure <$> f x

instance Applicative FreeBool where
  pure = Pure
  ff <*> fa = do
    f <- ff
    a <- fa
    pure $ f a

instance Monad FreeBool where
  (BAnd lhs rhs) >>= k = BAnd (lhs >>= k) (rhs >>= k)
  (BOr lhs rhs) >>= k = BOr (lhs >>= k) (rhs >>= k)
  (BNot fa) >>= k = BNot (fa >>= k)
  BFalse >>= _ = BFalse
  BTrue >>= _ = BTrue
  (Pure x) >>= k = k x
