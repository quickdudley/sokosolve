module Data.Heap (
  Heap,
  singleton,
  pop,
  fromList,
  toList
 ) where

import Data.Monoid (Monoid(..),(<>))

data Heap p a = Empty | Heap p a (Heap p a) (Heap p a)

instance (Ord p) => Monoid (Heap p a) where
  mempty = Empty
  mappend heap1@(Heap p1 a1 l1 r1) heap2@(Heap p2 a2 l2 r2)
    | p1 <= p2 = Heap p1 a1 (heap2 <> r1) l1
    | otherwise = Heap p2 a2 (heap1 <> r2) l2
  mappend heap Empty = heap
  mappend Empty heap = heap

instance (Show p, Ord p, Show a) => Show (Heap p a) where
  show h = "fromList " ++ show (toList h)

instance Functor (Heap p) where
  fmap _ Empty = Empty
  fmap f (Heap p a l r) = Heap p (f a) (fmap f l) (fmap f r)

singleton :: p -> a -> Heap p a
singleton p a = Heap p a Empty Empty

pop :: (Ord p) => Heap p a -> Maybe (p,a,Heap p a)
pop Empty = Nothing
pop (Heap p a l r) = Just (p,a,l <> r)

fromList :: (Ord p) => [(p,a)] -> Heap p a
fromList = tf . map (uncurry singleton) where
  tf [] = Empty
  tf [x] = x
  tf l = tf (mp l)
  mp [] = []
  mp [x] = [x]
  mp (a:b:r) = (a <> b) : mp r

toList heap = case pop heap of
  Nothing -> []
  Just (p,a,h) -> (p,a) : toList h

