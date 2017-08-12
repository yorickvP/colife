{-# LANGUAGE FlexibleInstances #-}
module Field (
    right, left, fromList, toList, U, U2, infinitize2,
    fromList2, bound2, showAll2, size2, shift, shift2
    ) where
import Data.Monoid ((<>))
import Control.Comonad
import Data.Functor.Compose
import Data.Functor.Classes
import Data.Distributive
import Control.Arrow ((&&&))
import Data.List (unfoldr)
-- The one-dimensional "Universe"
data U x = U [x] x [x]
	deriving (Show,Eq)
instance (Eq1 U) where
  liftEq eq (U a b c) (U d e f) = liftEq eq a d && eq b e && liftEq eq c f
--  deriving (Show,Eq)
type U2 x = Compose U U x
-- Instancing fmap for U
instance Functor U where
	fmap f (U a b c) = U (fmap f a) (f b) (fmap f c)

right, left :: (U a) -> (U a)
right (U a b []) = U [b] c ra
  where (c:ra) = reverse a
right (U a b (c:cs)) = U (b:a) c cs
left (U [] b c) = U rc a [b]
  where (a:rc) = reverse c
left (U (a:as) b c) = U as a (b:c)
-- instances from comments http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html
-- also: https://www.youtube.com/watch?v=F7F-BzOB670
-- instancing the comonad U
-- the tail is important
iterateU :: (a -> a) -> (a -> a) -> a -> U a
iterateU prev next x = U (tail $ iterate prev x) x (tail $ iterate next x)

instance Comonad U where
	extract (U _ a _) = a
	duplicate = iterateU left right

unfold :: (c -> (a,c)) -> (c -> a) -> (c -> (a,c)) -> c -> U a
unfold prev center next x =
   U (unfoldr (Just . prev) x) (center x) (unfoldr (Just . next) x)

instance Distributive U where
   distribute =
      unfold (fmap (extract . left) &&& fmap left)
             (fmap extract)
             (fmap (extract . right) &&& fmap right)

instance Monoid x => Monoid (U x) where
  mempty = U mempty mempty mempty
  mappend (U a b c) (U d e f) = U (a <> d) (b <> e) (c <> f)
instance (Comonad f, Comonad g, Distributive g) =>
  Comonad (Compose f g) where
        extract = extract . extract . getCompose
        duplicate x = fmap Compose $ Compose $
          fmap distribute $
          duplicate $ fmap duplicate $ getCompose x
--instance Foldable U where
--  foldr f z x = f (extract x) (foldr f z $ right x)
umap :: (a (b c) -> x (y z)) -> Compose a b c -> Compose x y z
umap f = Compose . f . getCompose
map2u :: Functor x => (a1 -> y z) -> (a (b c) -> x a1) -> Compose a b c -> Compose x y z
map2u f g = umap $ fmap f . g


toList :: U x -> [x]
toList u = fmap extract $ iterate right u
-- Auxiliary functions for shifting the one-dimensional automaton

shift :: Int -> U x -> U x
shift i u = (iterate (if i<0 then left else right) u) !! (abs i)

shift2 :: Int -> Int -> U2 x -> U2 x
shift2 x y = map2u (shift x) (shift y)

infinity :: Monoid x => U x
infinity = U (repeat mempty) mempty (repeat mempty)
infinitize2 :: Monoid x => U2 x -> U2 x
infinitize2 = map2u (<> infinity) (<> infinity)


bound :: (Int,Int) -> U x -> U x
bound (x,y) (U a b c) = U (take x a) b (take y c)

bound2 :: ((Int, Int), (Int, Int)) -> U2 x -> U2 x
bound2 (x, y) = map2u (bound x) (bound y)

size :: U x -> (Int, Int)
size (U a _ c) = (length a, length c)
totalsize :: U x -> Int
totalsize = (1+).(uncurry (+)).size
size2 :: U2 x -> ((Int, Int), (Int, Int))
size2 (Compose x) = (size x, (size $ extract x))
fromList :: [x] -> U x
fromList (a:b) = right $ U [] a b
fromList [] = error "can't make U from empty list"
fromList2 :: [[x]] -> U2 x
fromList2 = Compose . fromList . fmap fromList

showAll :: Show x => U x -> String
showAll x = concatMap show $ take (totalsize x) $ toList x
showAll2 :: Show x => U2 x -> String
showAll2 (Compose x) = unlines $ take (totalsize x) $ toList $ fmap showAll x
