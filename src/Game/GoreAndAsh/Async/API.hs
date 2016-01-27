{-|
Module      : Game.GoreAndAsh.Async.API
Description : Monadic and arrow API for module
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.Async.API(
    MonadAsync(..)
  ) where

import Control.Monad.Trans 

import Game.GoreAndAsh.Async.Module 

-- | Low level monadic API for module.
--
-- Note: does not require 'm' to be 'IO' monad.
class Monad m => MonadAsync m where 
  -- | stab method, temporary
  asyncStab :: m ()

instance {-# OVERLAPPING #-} Monad m => MonadAsync (AsyncT s m) where
  asyncStab = return ()

instance {-# OVERLAPPABLE #-} (Monad (mt m), MonadAsync m, MonadTrans mt) => MonadAsync (mt m) where 
  asyncStab = lift asyncStab