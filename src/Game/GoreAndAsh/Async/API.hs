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
  -- * Monadic API
    MonadAsync(..)
  , MonadAsyncExcepion(..)
  -- * Arrow API
  -- ** Not bounded async
  , asyncAction
  , asyncActionC
  , asyncActionEx
  , asyncActionExC
  -- ** Bounded async
  , asyncActionBound
  , asyncActionBoundC
  , asyncActionBoundEx
  , asyncActionBoundExC
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

import Game.GoreAndAsh.Core
import Game.GoreAndAsh.Async.Module 
import Game.GoreAndAsh.Async.State

-- | Exception of the async monadic API
data MonadAsyncExcepion =
    AsyncWrongType TypeRep TypeRep -- ^ Expected type doesn't match stored in async value
  | AsyncNotFound AsyncId -- ^ There is no async value with the id
  | SyncWrongType TypeRep TypeRep -- ^ Expected type doesn't match stored in sync value
  deriving (Generic, Show)

instance Exception MonadAsyncExcepion 

instance NFData MonadAsyncExcepion where 
  rnf e = case e of 
    AsyncWrongType tr1 tr2 -> rnfTypeRep tr1 `deepseq` rnfTypeRep tr2
    AsyncNotFound i -> i `deepseq` ()
    SyncWrongType tr1 tr2 -> rnfTypeRep tr1 `deepseq` rnfTypeRep tr2

-- | Low level monadic API for module.
--
-- Note: does not require 'm' to be 'IO' monad.
class (MonadIO m, MonadThrow m) => MonadAsync m where 
  -- | Start execution of 'IO' action concurrently and return its id
  asyncActionM :: Typeable a => IO a -> m AsyncId

  -- | Start execution of 'IO' action concurrently and return its id
  --
  -- Note: forks thread within same OS thread.
  asyncActionBoundM :: Typeable a => IO a -> m AsyncId

  -- | Check state of concurrent value
  --
  -- Could throw 'MonadAsyncExcepion', 'AsyncWrongType' and 'AsyncNotFound' constructors.
  asyncPollM :: Typeable a => AsyncId -> m (Maybe (Either SomeException a))

  -- | Stops given async execution
  asyncCancelM :: AsyncId -> m ()

  -- | Schedule action to be executed at the end of frame.
  --
  -- Use 'asyncSyncPollM' to get result at next frame.
  --
  -- Note: order of IO actions is preserved.
  asyncSyncActionM :: Typeable a => IO a -> m SyncId

  -- | Fires when given synchronious action is completed (at next frame after scheduling)
  --
  -- Could throw 'MonadAsyncExcepion', 'SyncWrongType' constructor.
  asyncSyncPollM :: Typeable a => SyncId -> m (Maybe (Either SomeException a))

  -- | Unshedule given action from execution
  --
  -- Actually you can unshedule it until end of frame when corresponding 'asyncSyncActionM'
  -- was called.
  asyncSyncCanceM :: SyncId -> m ()

instance {-# OVERLAPPING #-} (MonadIO m, MonadThrow m) => MonadAsync (AsyncT s m) where
  asyncActionM !io = do 
    av <- liftIO . async $! io 
    state $! registerAsyncValue av

  asyncActionBoundM !io = do 
    av <- liftIO . asyncBound $! io 
    state $! registerAsyncValue av

  asyncPollM :: forall a . Typeable a => AsyncId -> AsyncT s m (Maybe (Either SomeException a))
  asyncPollM i = do 
    mav <- getFinishedAsyncValue i <$> AsyncT get 
    case mav of 
      Nothing -> throwM . AsyncNotFound $! i 
      Just av -> case av of 
        Nothing -> return Nothing 
        Just ev -> case ev of 
          Left e -> return . Just . Left $! e
          Right da -> case fromDynamic da of 
            Nothing -> throwM $! AsyncWrongType (typeRep (Proxy :: Proxy a)) (dynTypeRep da)
            Just a -> return . Just . Right $! a

  asyncCancelM i = do 
    mav <- state $! cancelAsyncValue i
    case mav of 
      Nothing -> return ()
      Just av -> liftIO $! cancel av 

  asyncSyncActionM = state . registerSyncValue

  asyncSyncPollM :: forall a . Typeable a => SyncId -> AsyncT s m (Maybe (Either SomeException a))
  asyncSyncPollM i = do 
    mav <- getFinishedSyncValue i <$> AsyncT get 
    case mav of 
      Nothing -> return Nothing 
      Just ev -> case ev of 
        Left e -> return . Just . Left $! e
        Right da -> case fromDynamic da of 
          Nothing -> throwM $! SyncWrongType (typeRep (Proxy :: Proxy a)) (dynTypeRep da)
          Just a -> return . Just . Right $! a

  asyncSyncCanceM i = state $! ((), ) <$> cancelSyncValue i

instance {-# OVERLAPPABLE #-} (MonadIO (mt m), MonadThrow (mt m), MonadAsync m, MonadTrans mt) => MonadAsync (mt m) where 
  asyncActionM = lift . asyncActionM
  asyncActionBoundM = lift . asyncActionBoundM
  asyncPollM = lift . asyncPollM
  asyncCancelM = lift . asyncCancelM
  asyncSyncActionM = lift . asyncSyncActionM
  asyncSyncPollM = lift . asyncSyncPollM
  asyncSyncCanceM = lift . asyncSyncCanceM

-- | Execute given 'IO' action concurrently. Event fires once when the action 
-- is finished. Exceptions are rethrown into main thread.
asyncActionG :: (MonadAsync m, Typeable a) => 
  (IO a -> GameMonadT m AsyncId) -- ^ Maker of async value
  -> IO a -> GameWire m b (Event a)
asyncActionG mkAsync io = mkGen $ \_ _ -> do 
  i <- mkAsync io 
  return (Right NoEvent, go i)
  where
    go i = mkGen $ \_ _ -> do
      mr <- asyncPollM i 
      case mr of 
        Nothing -> return (Right NoEvent, go i)
        Just ea -> case ea of 
          Left e -> throwM e 
          Right a -> return (Right $ Event a, never)

-- | Execute given 'IO' action concurrently. Event fires once when the action 
-- is finished. Exceptions are rethrown into main thread.
--
-- The concurrent action can be canceled by input event.
asyncActionCG :: (MonadAsync m, Typeable a) =>
  (IO a -> GameMonadT m AsyncId) -- ^ Maker of async value
  -> IO a -> GameWire m (Event b) (Event a)
asyncActionCG mkAsync io = mkGen $ \_ ce -> case ce of 
  NoEvent -> do 
    i <- mkAsync io 
    return (Right NoEvent, go i)
  Event _ -> return (Right NoEvent, never)
  where
    go i = mkGen $ \_ ce -> case ce of  
      NoEvent -> do
        mr <- asyncPollM i 
        case mr of 
          Nothing -> return (Right NoEvent, go i)
          Just ea -> case ea of 
            Left e -> throwM e 
            Right a -> return (Right $ Event a, never)
      Event _ -> do 
        asyncCancelM i 
        return (Right NoEvent, never)

-- | Execute given 'IO' action concurrently. 
-- 
-- Event fires once when the action is finished. Exceptions in the concurrent
-- action are returned in event payload.
asyncActionExG :: (MonadAsync m, Typeable a) =>
  (IO a -> GameMonadT m AsyncId) -- ^ Maker of async value
  -> IO a -> GameWire m b (Event (Either SomeException a))
asyncActionExG mkAsync io = mkGen $ \_ _ -> do 
  i <- mkAsync io 
  return (Right NoEvent, go i)
  where
    go i = mkGen $ \_ _ -> do
      mr <- asyncPollM i 
      case mr of 
        Nothing -> return (Right NoEvent, go i)
        Just ea -> return (Right $ Event ea, never)

-- | Execute given 'IO' action concurrently. 
--
-- Event fires once when the action is finished. Exceptions in the 
-- concurrent action are returned in event payload.
--
-- The concurrent action can be canceled by input event.
asyncActionExCG :: (MonadAsync m, Typeable a) =>
  (IO a -> GameMonadT m AsyncId) -- ^ Maker of async value
  -> IO a -> GameWire m (Event b) (Event (Either SomeException a))
asyncActionExCG mkAsync io = mkGen $ \_ ce -> case ce of 
  NoEvent -> do 
    i <- mkAsync io 
    return (Right NoEvent, go i)
  Event _ -> return (Right NoEvent, never)
  where
    go i = mkGen $ \_ ce -> case ce of  
      NoEvent -> do
        mr <- asyncPollM i 
        case mr of 
          Nothing -> return (Right NoEvent, go i)
          Just ea -> return (Right $ Event ea, never)
      Event _ -> do 
        asyncCancelM i 
        return (Right NoEvent, never)

-- | Execute given 'IO' action concurrently. Event fires once when the action 
-- is finished. Exceptions are rethrown into main thread.
asyncAction :: (MonadAsync m, Typeable a) => IO a -> GameWire m b (Event a)
asyncAction = asyncActionG asyncActionM

-- | Execute given 'IO' action concurrently. Event fires once when the action 
-- is finished. Exceptions are rethrown into main thread.
--
-- The concurrent action can be canceled by input event.
asyncActionC :: (MonadAsync m, Typeable a) => IO a -> GameWire m (Event b) (Event a)
asyncActionC = asyncActionCG asyncActionM

-- | Execute given 'IO' action concurrently. 
-- 
-- Event fires once when the action is finished. Exceptions in the concurrent
-- action are returned in event payload.
asyncActionEx :: (MonadAsync m, Typeable a) => IO a -> GameWire m b (Event (Either SomeException a))
asyncActionEx = asyncActionExG asyncActionM

-- | Execute given 'IO' action concurrently. 
--
-- Event fires once when the action is finished. Exceptions in the 
-- concurrent action are returned in event payload.
--
-- The concurrent action can be canceled by input event.
asyncActionExC :: (MonadAsync m, Typeable a) => IO a -> GameWire m (Event b) (Event (Either SomeException a))
asyncActionExC = asyncActionExCG asyncActionM

-- | Execute given 'IO' action concurrently. Event fires once when the action 
-- is finished. Exceptions are rethrown into main thread.
--
-- Note: forks thread within same OS thread.
asyncActionBound :: (MonadAsync m, Typeable a) => IO a -> GameWire m b (Event a)
asyncActionBound = asyncActionG asyncActionBoundM

-- | Execute given 'IO' action concurrently. Event fires once when the action 
-- is finished. Exceptions are rethrown into main thread.
--
-- The concurrent action can be canceled by input event.
--
-- Note: forks thread within same OS thread.
asyncActionBoundC :: (MonadAsync m, Typeable a) => IO a -> GameWire m (Event b) (Event a)
asyncActionBoundC = asyncActionCG asyncActionBoundM

-- | Execute given 'IO' action concurrently. 
-- 
-- Event fires once when the action is finished. Exceptions in the concurrent
-- action are returned in event payload.
--
-- Note: forks thread within same OS thread.
asyncActionBoundEx :: (MonadAsync m, Typeable a) => IO a -> GameWire m b (Event (Either SomeException a))
asyncActionBoundEx = asyncActionExG asyncActionBoundM

-- | Execute given 'IO' action concurrently. 
--
-- Event fires once when the action is finished. Exceptions in the 
-- concurrent action are returned in event payload.
--
-- The concurrent action can be canceled by input event.
--
-- Note: forks thread within same OS thread.`
asyncActionBoundExC :: (MonadAsync m, Typeable a) => IO a -> GameWire m (Event b) (Event (Either SomeException a))
asyncActionBoundExC = asyncActionExCG asyncActionBoundM
