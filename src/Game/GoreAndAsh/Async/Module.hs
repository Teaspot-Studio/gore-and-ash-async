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

import Control.Monad.Catch
import Control.Monad.Fix 
import Control.Monad.State.Strict

import Game.GoreAndAsh
import Game.GoreAndAsh.Async.State

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
  runModule (AsyncT m) s = do
    ((a, s'), nextState) <- runModule (runStateT m s) (asyncNextState s)
    return (a, s' {
        asyncNextState = nextState 
      })  
  
  newModuleState = emptyAsyncState <$> newModuleState
  withModule _ = id
  cleanupModule _ = return ()