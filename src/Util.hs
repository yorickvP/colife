module Util ((<$$>)) where
(<$$>)  :: (Functor f, Functor f1) => (a -> b) -> f1 (f a) -> f1 (f b)
(<$$>) = fmap.fmap
infixl 4 <$$>
