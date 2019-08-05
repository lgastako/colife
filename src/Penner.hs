module Penner where

-- From Chris Penner's Comonads by Example talk (2/4)
-- Monadic Party

import           Control.Comonad.Store      ( Store
                                            , experiment
                                            , extend
                                            , extract
                                            , peek
                                            , store
                                            )
import           Data.Bool                  ( bool )
import           Data.List.Split            ( chunksOf )
import           Data.Monoid
import qualified Data.Set              as S

type Grid  = Store Coord Bool
type Coord = (Sum Int, Sum Int)

startingGrid :: Grid
startingGrid = store checkAlive (0, 0)
  where
    checkAlive :: (Sum Int, Sum Int) -> Bool
    checkAlive coord = S.member coord livingCells

    livingCells :: S.Set (Sum Int, Sum Int)
    livingCells = S.fromList (blinker `at` (2, 2))

step :: Grid -> Grid
step = extend checkCellAlive

checkCellAlive :: Grid -> Bool
checkCellAlive grid = case (extract grid, numLivingNeighbors grid) of
  (True, 2) -> True
  (_,    3) -> True
  _         -> False

numLivingNeighbors :: Grid -> Int
numLivingNeighbors = getSum . foldMap (bool 0 1) . experiment neighborLocations

neighborLocations :: Coord -> [Coord]
neighborLocations location = mappend location <$>
  [ (-1,  1), (-1, 0), (-1, -1)
  , ( 0, -1),          ( 0,  1)
  , ( 1, -1), ( 1, 0), ( 1,  1)
  ]

at :: [Coord] -> Coord -> [Coord]
coords `at` origin = map (<> origin) coords

drawGrid :: Int -> Grid -> String
drawGrid size g = unlines $ do
  x <- [-size..size-1]
  pure $ do
    y <- [-size..size-1]
    pure . bool '.' '#' $ peek (Sum x, Sum y) g

glider, blinker, beacon :: [Coord]
glider  = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = [(0, 0), (1, 0), (2, 0)]
beacon  = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]

-- drawGrid :: Int -> Grid -> String
-- drawGrid size = unlines
--   . map concat
--   . chunksOf (size * 2 + 1)
--   . fmap (bool "." "#")
--   . experiment spots
--   where
--     spots :: Coord -> [Coord]
--     spots location = mappend location <$>
--       [ (Sum x, Sum y)
--       | x <- [-size..size]
--       , y <- [-size..size]
--       ]
