module Main where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, extend)
import Effect (Effect)
import Effect.Console (log)

-- Context w (jazz trio), inside sound (a)
-- Get a sound out of my trio, get an a out of my w
-- Jazz trio, imagining my trio making a sound, get that point in the future

-- extract (Comonad)
-- w a -> a
-- pure (return, Monad)
-- a -> m a

-- expand (Comonad)
-- w a -> (w a -> a) -> w a
-- bind (Monad)
-- m a -> (a -> m a) -> m a

newtype MyCofree f a = MyCofree { playingNow :: a, inTheFuture :: Unit -> f (MyCofree f a) }

instance functorMyCofree :: Functor f => Functor (MyCofree f) where
  map f (MyCofree { playingNow, inTheFuture }) = MyCofree { playingNow: f playingNow, inTheFuture: (map <<< map <<< map) f inTheFuture }

instance extendMyCofree :: Functor f => Extend (MyCofree f) where
  extend f mcf@(MyCofree { inTheFuture }) = MyCofree { playingNow: f mcf, inTheFuture: (map <<< map) (extend f) inTheFuture }

instance comonadMyCofree :: Functor f => Comonad (MyCofree f) where
  extract (MyCofree { playingNow }) = playingNow

main :: Effect Unit
main = do
  log "🍝"
