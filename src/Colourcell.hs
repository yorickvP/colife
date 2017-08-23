module Colourcell (Colour(..), conway,randomCell) where
import Field (U, left,toList)
import Control.Comonad (extract)
import System.IO.Unsafe (unsafePerformIO)
import Data.Functor.Compose
import System.Console.Terminfo hiding (col)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Control.Monad.Random

max_age :: Int
max_age = 5
{-# NOINLINE terminal #-}
terminal :: Terminal
terminal = unsafePerformIO $ setupTermFromEnv

colStr :: (Int, Int, Int) -> String
colStr (r, g, b) = fromJust $ getCapability terminal $ setForegroundColor <*> pure (ColorNumber (16 + (36 * (r `quot` 43)) + (6 * (g `quot` 43)) + b `quot` 43))

data CellColor = CellColor { hue :: Float, sat :: Float }
               deriving (Show, Eq)
toRGB :: CellColor -> (Int, Int, Int)
toRGB (CellColor hue sat) = addm $ case h1 of
  0 -> (c, x, 0)
  1 -> (x, c, 0)
  2 -> (0, c, x)
  3 -> (0, x, c)
  4 -> (x, 0, c)
  5 -> (c, 0, x)
  _ -> error $ show hue
  where
    chroma = 1.0 * sat
    h1 = (floor (hue / 60)) :: Int
    x = truncate $ 255 * (chroma * fromIntegral (1 - abs (h1 `mod` 2 - 1))) :: Int
    c = truncate $ 255 * chroma
    m = 255 - c
    addm (r, g, b) = (r + 255 - c, g + 255 - c, b + 255 - c)

darken :: Float -> (Int, Int, Int) -> (Int, Int, Int)
darken v (r, g, b) = (d r, d g, d b)
  where d x = round $ v * fromIntegral x

data Colour = AliveCell { col :: CellColor }
            | DyingCell { col :: CellColor, age :: Int }
            | DeadCell
            deriving (Eq)
alive :: Colour -> Bool
alive (AliveCell _) = True
alive _ = False
instance Show Colour where
  show DeadCell = " "
  show (AliveCell col) = (colStr $ toRGB col) <> "o"
  show (DyingCell col age) = (colStr $ darken (0.3 - ((fromIntegral age)/(fromIntegral max_age)*0.3)) $ toRGB col) <> "o"
blend :: [CellColor] -> CellColor
blend cols = CellColor newhue newsat
  where
    toPlane (CellColor hue sat) = (sat * (cos (hue / 360)), sat * (sin (hue / 360)))
    add (a,b) (c,d) = (a+c, b+d)
    divBy c (a,b) = (a/c, b/c)
    len = length cols
    avgplane = divBy (fromIntegral len) $ foldl add (0,0) (map toPlane cols)
    newsat = sqrt ((fst avgplane) ** 2 + (snd avgplane) ** 2)
    newhue = if newsat == 0 then 0 else (uncurry atan2 avgplane) / 2 / pi * 360
instance Monoid Colour where
  mempty = DeadCell
  mappend DeadCell DeadCell = mempty
  mappend DeadCell x = x
  mappend x DeadCell = x
  mappend (DyingCell h1 a1) (DyingCell h2 a2)
    | (a1 + a2 >= max_age) = DeadCell
    | otherwise       = DyingCell (blend [h1, h2]) (max_age - a1 - a2)
  mappend (DyingCell _ _) (AliveCell h2) = AliveCell h2
  mappend (AliveCell h2) (DyingCell _ _) = AliveCell h2
  mappend (AliveCell h1) (AliveCell h2) = AliveCell (blend [h1, h2])

-- Conway's game of life rule (believe it).
conway :: Compose U U Colour -> Colour
conway field = case extract field of
  AliveCell col -> if n == 3 || n == 4 then AliveCell col else DyingCell col 0
  DyingCell col age -> if n == 3 then AliveCell avghue else
    (if (age == max_age) then DeadCell else (DyingCell col (age+1)))
  DeadCell -> if n == 3 then AliveCell avghue else DeadCell
  where
    around = take 3 . toList . left
    -- around (U (a:_) b (c:_)) = [a,b,c]
    neighbours = filter alive $ concatMap around $ around $ getCompose field
    n = length $ neighbours
    avghue = blend $ map col neighbours
wolfram :: Int -> U Colour -> Colour
wolfram _ = undefined
randomCell :: (MonadRandom m) => Float -> m Colour
randomCell chance = do
  life <- (<chance) <$> getRandom
  if life then AliveCell <$> (CellColor <$> (getRandomR (0, 360)) <*> getRandom)
    else return DeadCell
