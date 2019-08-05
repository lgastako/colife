{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Penner where

-- From Chris Penner's Comonads by Example talk (2/4)
-- Monadic Party

import Control.Arrow             ( first )
import Control.Comonad           ( extend
                                 , extract
                                 )
import Control.Comonad.MemoStore ( Store
                                 , experiment
                                 , peek
                                 , store
                                 )
import Data.Bool                 ( bool )
import Data.MemoTrie             ( (:->:)
                                 , HasTrie
                                 , Reg
                                 , enumerate
                                 , enumerateGeneric
                                 , trie
                                 , trieGeneric
                                 , untrie
                                 , untrieGeneric
                                 )
import Data.Monoid               ( Sum( Sum )
                                 , getSum
                                 )
import Data.Set                  ( Set
                                 , fromList
                                 , member
                                 )

type Grid  = Store Coord Bool
type Coord = (Sum Int, Sum Int)

startingGrid :: Grid
startingGrid = store checkAlive (0, 0)
  where
    checkAlive :: Coord -> Bool
    checkAlive coord = member coord livingCells

    livingCells :: Set Coord
    livingCells = fromList
      $ blinker `at` (-3, -3)
      ++ glider `at` (2, 0)

-- instance HasTrie (Sum Int) where
--   newtype Sum Int :->: a = IntTrie (Word :->: a)

--   enumerate :: forall b. (Sum Int :->: b) -> [(Sum Int, b)]
--   enumerate (IntTrie t) = enum' fromIntegral t

--   trie :: forall b. (Sum Int -> b) -> Sum Int :->: b
--   trie f = IntTrie (trie (f . fromIntegral))

--   untrie :: forall b. (Sum Int :->: b) -> Sum Int -> b
--   untrie (IntTrie t) n = untrie t (fromIntegral . getSum $ n)

-- enum' :: (HasTrie a) => (a -> a') -> (a :->: b) -> [(a', b)]
-- enum' f = (fmap . first) f . enumerate


instance HasTrie (Sum Int) where
  newtype (Sum Int :->: b) = SumIntTrie { unSumIntTrie :: Reg (Sum Int) :->: b }
  trie = trieGeneric SumIntTrie
  untrie = untrieGeneric unSumIntTrie
  enumerate = enumerateGeneric unSumIntTrie

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
coords `at` origin = map (origin <>) coords

drawGrid :: Int -> Grid -> String
drawGrid size g = (++ divider) . unlines $ do
  x <- [-size..size-1]
  pure $ do
    y <- [-size..size-1]
    pure . bool '.' '#' $ peek (Sum x, Sum y) g
  where
    divider = replicate (size * 2) '=' ++ "\n"

glider, blinker, beacon :: [Coord]
glider  = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = [(0, 0), (1, 0), (2, 0)]
beacon  = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]

run :: Int -> IO ()
run n = putStr
  . unlines
  . init
  . concatMap (lines . drawGrid 5)
  . take n
  . iterate step
  $ startingGrid

-- import Data.List.Split ( chunksOf )
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
