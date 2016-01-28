{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Game.GoreAndAsh.Async.Module
Description : Monad transformer and instance for core module
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.Async.Module(
    AsyncT(..)
  ) where

import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Monad.Fix 
import Control.Monad.State.Strict

import Game.GoreAndAsh
import Game.GoreAndAsh.Async.State

import qualified Data.Foldable as F  
import qualified Data.HashMap.Strict as H
import qualified Data.Sequence as S 

-- | Monad transformer of async core module.
--
-- [@s@] - State of next core module in modules chain;
--
-- [@m@] - Next monad in modules monad stack;
--
-- [@a@] - Type of result value;
--
-- How to embed module:
-- 
-- @
-- type AppStack = ModuleStack [AsyncT, ... other modules ... ] IO
--
-- newtype AppMonad a = AppMonad (AppStack a)
--   deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadThrow, MonadCatch, MonadAsync)
-- @
--
-- The module is NOT pure within first phase (see 'ModuleStack' docs), therefore currently only 'IO' end monad can handler the module.
newtype AsyncT s m a = AsyncT { runAsyncT :: StateT (AsyncState s) m a }
  deriving (Functor, Applicative, Monad, MonadState (AsyncState s), MonadFix, MonadTrans, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance GameModule m s => GameModule (AsyncT s m) (AsyncState s) where 
  type ModuleState (AsyncT s m) = AsyncState s
  runModule (AsyncT m) s1 = do
    ((a, s2), nextState) <- runModule (runStateT m s1) (asyncNextState s1)
    s3 <- pollAsyncs . purgeAsyncs $! s2 
    s4 <- liftIO . execSyncs . purgeSyncs $! s3
    return (a, s4 {
        asyncNextState = nextState 
      })
  
  newModuleState = emptyAsyncState <$> newModuleState
  withModule _ = id
  cleanupModule _ = return ()

-- | Polls all async values and update values
pollAsyncs :: MonadIO m => AsyncState s -> m (AsyncState s)
pollAsyncs s = do
  mp <- mapM pollVal . asyncAValues $! s 
  return $! s {
      asyncAValues = mp
    }
  where
  pollVal ev = case ev of 
    Left a -> do 
      mr <- liftIO . poll $! a
      case mr of 
        Nothing -> return ev 
        Just r -> return . Right $! r
    _ -> return ev

-- | Execute all sync values
execSyncs :: AsyncState s -> IO (AsyncState s)
execSyncs s = do 
  as <- mapM (uncurry execAction) . asyncScheduled $! s
  return $! s {
      asyncScheduled = S.empty 
    , asyncSValues = F.foldl' (\acc (k, v) -> H.insert k v acc) H.empty as
    }
  where 
    execAction i io = (fmap ((i,) . Right) io) `catchAll` (return . (i,) . Left)