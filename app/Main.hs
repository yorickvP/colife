module Main where

import Lib
import Control.Comonad ((=>>))
import Control.Concurrent (threadDelay)
glider :: U2 Binary
glider = bound2 ((10,10), (10,10)) $ infinitize2 $ fromList2 ([
  [O,O,O,O,O],
  [O,O,X,O,O],
  [O,O,O,X,O],
  [O,X,X,X,O],
  [O,O,O,O,O]] :: [[Binary]])
runField :: U2 Binary -> IO ()
runField field = do
  putStr $ "\ESC[H"
  putStr $ showAll2 field
  threadDelay 100000
  let new = bound2 (size2 field) $ field =>> conway
  if field == new then return () else runField new

main :: IO ()
main = runField glider
