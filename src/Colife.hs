{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

module Colife where

import Control.Comonad     ( (<@>)
                           , Comonad
                           , ComonadApply
                           , duplicate
                           , extract
                           )
import Control.Concurrent  ( threadDelay )
import System.Console.ANSI ( clearScreen )
import System.Environment  ( getArgs )

-- From https://gist.github.com/gatlin/21d669321c617836a317693aef63a3c3

class LeftRight t where
  left  :: t a -> t a
  right :: t a -> t a

class UpDown t where
  up   :: t a -> t a
  down :: t a -> t a

-- | An infinite list of values
data Stream a = a :> Stream a
  deriving Functor

instance Foldable Stream where
  foldr f z (x :> xs) = foldr f (f x z) xs

-- * Stream utilities

-- | Allows finitary beings to view slices of a 'Stream'
takeS :: Int -> Stream a -> [a]
takeS 0 _         = []
takeS n (x :> xs) = x : takeS (n-1) xs

takeWhileChanging :: Int -> Stream (Sheet Cell) -> [Sheet Cell]
takeWhileChanging sz' (x :> xs@(y :> _))
  | sheetToList sz x == sheetToList sz y = [x]
  | otherwise = x:takeWhileChanging sz xs
  where
    sz = sz' `div` 2

-- | Build a 'Stream' from a generator function
unfoldS :: (b -> (a, b)) -> b -> Stream a
unfoldS f c = x :> unfoldS f d
  where
    (x, d) = f c

-- | Build a 'Stream' by mindlessly repeating a value
repeatS :: a -> Stream a
repeatS x = x :> repeatS x

-- | Build a 'Stream' from a finite list, filling in the rest with a designated
-- default value
fromS :: a -> [a] -> Stream a
fromS def = foldr (:>) (repeatS def)

-- | Prepend values from a list to an existing 'Stream'
prependList :: [a] -> Stream a -> Stream a
prependList [] str = str
prependList xs str = foldr (:>) str xs

-- | A 'Stream' is a comonad
instance Comonad Stream where
  extract      (a :> _)  = a
  duplicate s@(_a :> as) = s :> duplicate as

-- | It is also a 'ComonadApply'
instance ComonadApply Stream where
  (f :> fs) <@> (x :> xs) = f x :> (fs <@> xs)

-- * Tapes, our workhorse

-- | A 'Tape' is always focused on one value in a 1-dimensional infinite stream
data Tape a = Tape (Stream a) a (Stream a)
  deriving Functor

-- | We can go left and right along a tape!
instance LeftRight Tape where
  left  (Tape (l :> ls) c rs) = Tape ls l (c :> rs)
  right (Tape ls c (r :> rs)) = Tape (c :> ls) r rs

-- | Build out the left, middle, and right parts of a 'Tape' with generator
-- functions.
unfoldT :: (c -> (a, c)) -> (c -> a) -> (c -> (a, c)) -> c-> Tape a
unfoldT prev center next = Tape
  <$> unfoldS prev
  <*> center
  <*> unfoldS next

-- | A simplified unfolding mechanism that will be useful shortly
tapeIterate :: (a -> a) -> (a -> a) -> a -> Tape a
tapeIterate prev next = unfoldT (dup . prev) id (dup . next)
  where
    dup a = (a, a)

-- | Create a 'Tape' from a list where everything to the left is some default
-- value and the list is the focused value and everything to the right.
tapeFromList :: a -> [a] -> Tape a
tapeFromList def xs = right $ Tape background' def $ fromS def xs
  where
    background' = repeatS def

tapeToList :: Int -> Tape a -> [a]
tapeToList n (Tape ls x rs) = reverse (takeS n ls) ++ [x] ++ takeS n rs

instance Comonad Tape where
  extract (Tape _ c _) = c
  duplicate            = tapeIterate left right

instance ComonadApply Tape where
  Tape fl fc fr <@> Tape xl xc xr = Tape (fl <@> xl) (fc xc) (fr <@> xr)

-- * Sheets! We're so close!

-- | A 2-dimensional 'Tape' of 'Tape's.
newtype Sheet a = Sheet (Tape (Tape a))
  deriving Functor

instance UpDown Sheet where
  up   (Sheet p) = Sheet (left p)
  down (Sheet p) = Sheet (right p)

instance LeftRight Sheet where
  left  (Sheet p) = Sheet (left  <$> p)
  right (Sheet p) = Sheet (right <$> p)

-- Helper functions to take a given 'Sheet' and make an infinite 'Tape' of it.
horizontal :: Sheet a -> Tape (Sheet a)
horizontal = tapeIterate left right

vertical :: Sheet a -> Tape (Sheet a)
vertical = tapeIterate up down

instance Comonad Sheet where
  extract (Sheet p) = extract $ extract p
  -- | See? Told you 'tapeIterate' would be useful
  duplicate s = Sheet $ horizontal <$> vertical s

instance ComonadApply Sheet where
  Sheet (Tape fu fc fd) <@> Sheet (Tape xu xc xd) =
    Sheet $ Tape (fu `ap` xu) (fc <@> xc) (fd `ap` xd)
    where
      ap t1 t2 = fmap (<@>) t1 <@> t2

-- | Produce a 'Sheet' from a list of lists, where each inner list is a row.
sheet :: a -> [[a]] -> Sheet a
sheet def grid = Sheet cols
  where
    cols = fmap (tapeFromList def) rows
    rows = tapeFromList [def] grid

-- | Slice a sheet for viewing purposes.
sheetToList :: Int -> Sheet a -> [[a]]
sheetToList n (Sheet zs) = tapeToList n (tapeToList n <$> zs)

-- | Construct an infinite sheet in all directions from a base value.
background :: a -> Sheet a
background x = Sheet . duplicate $ Tape (repeatS x) x (repeatS x)

printSheet :: Int -> Sheet Cell -> IO ()
printSheet n sh = mapM_ renderRow (sheetToList (n `div` 2) sh)

renderRow :: [Cell] -> IO ()
renderRow = putStrLn . concatMap renderCell
  where
    renderCell X = "."
    renderCell O = "*"

-- | A cell can be alive ('O') or dead ('X').
data Cell = X | O
  deriving (Eq, Ord, Read, Show)

-- | The first list is of numbers of live neighbors to make a dead 'Cell' become
-- alive; the second is numbers of live neighbors that keeps a live 'Cell'
-- alive.
type Ruleset = ([Int], [Int])

count :: Cell -> Int
count X = 0
count O = 1

conwayRules :: Ruleset
conwayRules = ([3], [2, 3])

executeRules :: Ruleset -> Sheet Cell -> Cell
executeRules (born, persist) s = go (extract s)
  where
    -- there is probably a more elegant way of getting all 8 neighbors
    neighbors =
      [ s & up            & extract
      , s & down          & extract
      , s & left          & extract
      , s & right         & extract
      , s & up    & right & extract
      , s & up    & left  & extract
      , s & down  & left  & extract
      , s & down  & right & extract
      ]
    liveCount = sum $ map count neighbors
    go O | liveCount `elem` persist = O
         | otherwise                = X
    go X | liveCount `elem` born    = O
         | otherwise                = X

type Board = [[Cell]]

-- TODO: random boards w/ various properties

-- | A 'Stream' of Conway games.
timeline :: Ruleset -> Sheet Cell -> Stream (Sheet Cell)
timeline rules = go
  where
    go ig  = ig :> go (rules' <@> duplicate ig)
    rules' = background $ executeRules rules

data Config = Config
  { maxIterations :: Maybe Int
  , delay         :: Int
  , size          :: Int
  , startingBoard :: Board
  } deriving (Eq, Ord, Read, Show)

configFromArgs :: [String] -> Config
configFromArgs = \case
  []  -> defaultConfig
  [b] -> defaultConfig { startingBoard = boardFromArg b }
  _   -> undefined
  where
    defaultConfig = Config
      { maxIterations = Just 1000
      , size          = 25
      , delay         = 0
      , startingBoard = rPentomino
      }

boardFromArg :: String -> Board
boardFromArg = \case
  "glider" -> glider
  "gun"    -> gliderGun
  "r"      -> rPentomino
  _        -> []

main :: IO ()
main = do
  config <- configFromArgs <$> getArgs
  print config
  runConfig config

runConfig :: Config -> IO ()
runConfig config = case maxIterations config of
  Nothing -> mapM_ renderOnce . takeWhileChanging (size config) $ tl
  Just n  -> mapM_ renderOnce . takeS n $ tl
  where
    tl = timeline conwayRules $ sheet X (startingBoard config)
    renderOnce s = do
      clearScreen
      printSheet (size config) s
      threadDelay $ delay config * 1000

-- ================================================================
--   Boards
-- ================================================================

boards :: [Board]
boards =
  [ gliderGun
  , glider
  , rPentomino
  ]

gliderGun :: Board
gliderGun =
  [ [X,X,X,X,O]
  , [X,O,O,O,O]
  , [O,O,O,O,X]
  , [O,X,X,O,X]
  , [O,O,O,O,X]
  , [X,O,O,O,O]
  , [X,X,X,X,O]
  ]

glider :: Board
glider =
  [ [ X, X, O ]
  , [ O, X, O ]
  , [ X, O, O ]
  ]

rPentomino :: Board
rPentomino =
  [ [ X, O, O ]
  , [ O, O, X ]
  , [ X, O, X ]
  ]

-- ================================================================
--   Helpers
-- ================================================================

(&) :: a -> (a -> b) -> b
(&) = flip ($)
