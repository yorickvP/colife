module Field (
    right, left, fromList, toList, U, U2(..), infinitize2,
    fromList2, bound2, showAll2, size2, shift, shift2
    ) where
import Data.Monoid ((<>))
import Control.Comonad
-- The one-dimensional "Universe"
data U x = U [x] x [x]
	deriving (Show,Eq)

-- The two-dimensional "Universe", I hope.
data U2 x = U2 (U (U x))
  deriving (Show,Eq)

-- Instancing fmap for U
instance Functor U where
	fmap f (U a b c) = U (fmap f a) (f b) (fmap f c)

right, left :: (U a) -> (U a)
right (U a b [c]) = U [b] c (reverse a)
right (U a b (c:cs)) = U (b:a) c cs
left (U [a] b c) = U (reverse c) a [b]
left (U (a:as) b c) = U as a (b:c)
-- todo: empty lists on the sides.
-- instances from comments http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html
-- instancing the comonad U
instance Comonad U where
	extract (U _ a _) = a
	duplicate a = U (tail $ iterate left a) a (tail $ iterate right a)

-- instancing the functor U2
instance Functor U2 where
	fmap f (U2 u) = U2 $ fmap (fmap f) u
instance Monoid x => Monoid (U x) where
  mempty = U mempty mempty mempty
  mappend (U a b c) (U d e f) = U (a <> d) (b <> e) (c <> f)
-- instancing U2 as a comonad
instance Comonad U2 where
	extract (U2 (U _ (U _ a _) _)) = a
	duplicate (U2 u) = fmap U2 $ U2 $ roll $ roll u where
		iterate1 f = tail.iterate f
		roll a = U (iterate1 (fmap left) a) a (iterate1 (fmap right) a)
umap f (U2 x) = U2 (f x)

toList :: U x -> [x]
toList u = fmap extract $ iterate right u
-- Auxiliary functions for shifting the one-dimensional automaton


shift :: Int -> U x -> U x
shift i u = (iterate (if i<0 then left else right) u) !! (abs i)

shift2 :: Int -> Int -> U2 x -> U2 x
shift2 x y = mapU2 (shift x) (shift y)

mapU2 :: (U (U y) -> (U (U z))) -> (U x -> U y) -> U2 x -> U2 z
mapU2 f g = umap $ f . fmap g
map2u :: (a -> U x) -> (U (U t) -> U a) -> U2 t -> U2 x
map2u f g = umap $ fmap f . g
infinity :: Monoid x => U x
infinity = U (repeat mempty) mempty (repeat mempty)
infinitize2 :: Monoid x => U2 x -> U2 x
infinitize2 = map2u (<> infinity) (<> infinity)


bound :: (Int,Int) -> U x -> U x
bound (x,y) (U a b c) = U (take x a) b (take y c)

bound2 :: ((Int, Int), (Int, Int)) -> U2 x -> U2 x
bound2 (x, y) = mapU2 (bound x) (bound y)

size :: U x -> (Int, Int)
size (U a _ c) = (length a, length c)
totalsize :: U x -> Int
totalsize = (1+).(uncurry (+)).size
size2 :: U2 x -> ((Int, Int), (Int, Int))
size2 (U2 x) = (size x, (size $ extract x))
fromList :: [x] -> U x
fromList (a:b) = right $ U [] a b
fromList [] = error "can't make U from empty list"
fromList2 :: [[x]] -> U2 x
fromList2 = U2 . fromList . fmap fromList

showAll :: Show x => U x -> String
showAll x = concatMap show $ take (totalsize x) $ toList x
showAll2 :: Show x => U2 x -> String
showAll2 (U2 x) = unlines $ take (totalsize x) $ toList $ fmap showAll x
