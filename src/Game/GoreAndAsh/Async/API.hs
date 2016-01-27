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
  , MonadAsyncExcepion(..)
  ) where

import Control.Concurrent.Async 
import Control.DeepSeq
import Control.Exception 
import Control.Monad.Catch 
import Control.Monad.IO.Class 
import Control.Monad.State.Strict
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Dynamic
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
  -- Could throw 'MonadAsyncExcepion'.
  asyncPollM :: Typeable a => AsyncId -> m (Event (Either SomeException a))

  -- | Stops given async execution
  asyncCancelM :: AsyncId -> m ()

instance {-# OVERLAPPING #-} (MonadIO m, MonadThrow m) => MonadAsync (AsyncT s m) where
  asyncEventM !io = do 
    av <- liftIO . async $! io 
    state $! registerAsyncValue av

  asyncEventBoundM !io = do 
    av <- liftIO . asyncBound $! io 
    state $! registerAsyncValue av

  asyncPollM :: forall a . Typeable a => AsyncId -> AsyncT s m (Event (Either SomeException a))
  asyncPollM i = do 
    mav <- getFinishedAsyncValue i <$> AsyncT get 
    case mav of 
      Nothing -> throwM . AsyncNotFound $! i 
      Just av -> case av of 
        Nothing -> return NoEvent 
        Just ev -> case ev of 
          Left e -> return . Event . Left $! e
          Right da -> case fromDynamic da of 
            Nothing -> throwM $! AsyncWrongType (typeRep (Proxy :: Proxy a)) (dynTypeRep da)
            Just a -> return . Event . Right $! a

  asyncCancelM i = do 
    mav <- state $! cancelAsyncValue i
    case mav of 
      Nothing -> return ()
      Just av -> liftIO $! cancel av 

instance {-# OVERLAPPABLE #-} (MonadIO (mt m), MonadThrow (mt m), MonadAsync m, MonadTrans mt) => MonadAsync (mt m) where 
  asyncEventM = lift . asyncEventM
  asyncEventBoundM = lift . asyncEventBoundM
  asyncPollM = lift . asyncPollM
  asyncCancelM = lift . asyncCancelM