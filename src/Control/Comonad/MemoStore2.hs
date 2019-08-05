{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Control.Comonad.MemoStore2
  ( Store
  , experiment
  , store
  , peek
  ) where

import           Control.Comonad             ( Comonad
                                             , extend
                                             , extract
                                             )
import qualified Control.Comonad.Store  as X
import           Control.Monad.Identity      ( Identity( Identity ) )
import           Data.MemoTrie               ( HasTrie
                                             , memo
                                             )

data Store s a = Store (s -> a) s

instance HasTrie s => Functor (Store s) where
  fmap f (Store g s) = Store (memo $ f . g) s

instance HasTrie s => Comonad (Store s) where
  extract (Store f a) = f a
  duplicate (Store f s) = Store (Store f) s

experiment :: forall f s a. Functor f => (s -> f s) -> Store s a -> f a
experiment f (Store wf s) = fmap wf . f $ s
  where
    _ = f  :: (s -> f s)
    _ = wf :: s -> a
    _ = s  :: s

-- experiment f (Store wf s) = extract wf <$> f s

peek :: forall s a. s -> Store s a -> a
-- peek s (Store g _) = extract g s
peek s (Store g _) = g s
  where
    _ = s :: s
    _ = g :: s -> a

store :: (s -> a) -> s -> Store s a
store f s = Store f s

-- data StoreT s w a = StoreT (w (s -> a)) s

-- type Store s = StoreT s Identity

-- instance (HasTrie s, Functor w) => Functor (StoreT s w) where
--   fmap f (StoreT wf s) = StoreT (fmap (memo . (f .)) wf) s

-- instance (HasTrie s, Comonad w) => Comonad (StoreT s w) where
--   duplicate (StoreT wf s) = StoreT (extend StoreT wf) s
--   extract   (StoreT wf s) = extract wf s

--  extend f  (StoreT wf s) = StoreT (extend (\wf' s' -> f (StoreT wf' s')) wf) s

-- experiment :: (Comonad w, Functor f) => (s -> f s) -> StoreT s w a -> f a
-- experiment f (StoreT wf s) = extract wf <$> f s

-- peek :: Comonad w => s -> StoreT s w a -> a
-- peek s (StoreT g _) = extract g s

-- store :: (s -> a) -> s -> Store s a
-- store f s = StoreT (Identity f) s
