module Main where

import Lib
import Control.Comonad ((=>>))
import Data.Maybe (fromJust)
import Control.Concurrent (threadDelay)
import System.Console.Terminfo
import Control.Monad (replicateM)

-- glider :: U2 Binary
-- glider = bound2 ((10,10), (10,10)) $ infinitize2 $ fromList2 ([
--   [O,O,O,O,O],
--   [O,O,X,O,O],
--   [O,O,O,X,O],
--   [O,X,X,X,O],
--   [O,O,O,O,O]] :: [[Binary]])

randomField :: IO (U2 Colour)
randomField = fromList2 <$> (replicateM 10 $ replicateM 30 $ randomCell 0.35)
runField :: Terminal -> U2 Colour -> IO ()
runField term field = do
  runTermOutput term $ fromJust $ getCapability term cursorHome
  putStr $ showAll2 field
  threadDelay 100000
  let new = bound2 (size2 field) $ field =>> conway
  if field == new then return () else runField term new
  --runField new

main :: IO ()
main = do
  t <- setupTermFromEnv
  f <- randomField
  runField t f
  
