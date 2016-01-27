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

import Control.Concurrent.Async 
import Control.DeepSeq
import Control.Exception 
import Control.Monad.Catch 
import Control.Monad.IO.Class 
import Control.Monad.State.Strict
import Control.Wire
import Data.Typeable
import GHC.Generics (Generic)
import Prelude hiding (id, (.))

import Game.GoreAndAsh.Async.Module 
import Game.GoreAndAsh.Async.State

-- | Exception of the async monadic API
data MonadAsyncExcepion =
    AsyncWrongType TypeRep TypeRep -- ^ Expected type doesn't match stored in async value
  | AsyncNotFound AsyncId -- ^ There is no async value with the id
  deriving (Generic, Show)

instance Exception MonadAsyncExcepion 

instance NFData MonadAsyncExcepion where 
  rnf e = case e of 
    AsyncWrongType tr1 tr2 -> rnfTypeRep tr1 `deepseq` rnfTypeRep tr2
    AsyncNotFound i -> i `deepseq` ()

-- | Low level monadic API for module.
--
-- Note: does not require 'm' to be 'IO' monad.
class (MonadIO m, MonadThrow m) => MonadAsync m where 
  -- | Start execution of 'IO' action concurrently and return its id
  asyncEventM :: Typeable a => IO a -> m AsyncId

  -- | Start execution of 'IO' action concurrently and return its id
  --
  -- Note: forks thread within same OS thread.
  asyncEventBoundM :: Typeable a => IO a -> m AsyncId

  -- | Check state of concurrent value
  --
  -- Could also return 'MonadAsyncExcepion' as 'Event' payload.
  asyncPollM :: AsyncId -> m (Event (Either SomeException a))

  -- | Check state of concurrent value
  --
  -- Could throw 'MonadAsyncExcepion'.
  asyncWaitM :: AsyncId -> m a

instance {-# OVERLAPPING #-} (MonadIO m, MonadThrow m) => MonadAsync (AsyncT s m) where
  asyncEventM !io = do 
    av <- liftIO . async $! io 
    state $! registerAsyncValue av

  asyncEventBoundM !io = do 
    av <- liftIO . asyncBound $! io 
    state $! registerAsyncValue av

  asyncPollM = fail "unimplemented"
  asyncWaitM = fail "unimplemented"

instance {-# OVERLAPPABLE #-} (MonadIO (mt m), MonadThrow (mt m), MonadAsync m, MonadTrans mt) => MonadAsync (mt m) where 
  asyncEventM = lift . asyncEventM
  asyncEventBoundM = lift . asyncEventBoundM
  asyncPollM = lift . asyncPollM
  asyncWaitM = lift . asyncWaitM