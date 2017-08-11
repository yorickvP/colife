module Cell (Binary(..), conway, wolfram) where
import Field (U, U2(..),left,toList)
import Control.Comonad (extract)
data Binary = X | O deriving (Eq)
instance Show Binary where
  show X = "#"
  show O = " "
alive :: Binary -> Bool
alive = (== X)
instance Monoid Binary where
  mempty = O
  mappend O O = O
  mappend _ _ = X

-- Conway's game of life rule (believe it).
conway :: U2 Binary -> Binary
conway (U2 x) = if (self && (n== 3||n == 4)) || ((not self) && n == 3) then X else O
  where
    around = take 3 . toList . left
    -- around (U (a:_) b (c:_)) = [a,b,c]
    n = length $ filter alive $ concatMap around $ around x
    self = alive $ extract (U2 x)
wolfram :: Int -> U Binary -> Binary
wolfram _ = undefined
