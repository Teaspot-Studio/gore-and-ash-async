{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-|
Module      : Game.GoreAndAsh.Async
Description : Core module for embedding concurrent IO actions into main loop.
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The core module contains API for embedding concurrent IO actions (async and sync) into main game loop for Gore&Ash.

The module does not depend on any other core modules, so 'AsyncT' could be placed at any place in monad stack.

The module is NOT pure within first phase (see 'ModuleStack' docs), therefore currently only 'IO' end monad can handler the module.

Example of embedding:

@
-- | Application monad is monad stack build from given list of modules over base monad (IO or Identity)
type AppStack = ModuleStack [AsyncT ... other modules ... ] IO
newtype AppState = AppState (ModuleState AppStack)
  deriving (Generic)

instance NFData AppState 

-- | Wrapper around type family
newtype AppMonad a = AppMonad (AppStack a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadThrow, MonadCatch, MonadAsync ... other modules monads ... )

instance GameModule AppMonad AppState where 
  type ModuleState AppMonad = AppState
  runModule (AppMonad m) (AppState s) = do 
    (a, s') <- runModule m s 
    return (a, AppState s')
  newModuleState = AppState <$> newModuleState
  withModule _ = withModule (Proxy :: Proxy AppStack)
  cleanupModule (AppState s) = cleanupModule s 

-- | Arrow that is build over the monad stack
type AppWire a b = GameWire AppMonad a b
@

-}
module Game.GoreAndAsh.Async(
  -- * Low-level
    AsyncState
  , AsyncT 
  , AsyncId
  , MonadAsync(..)
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
  -- ** Sync actions
  , asyncSyncAction
  , asyncSyncActionEx
  , asyncSyncActionC
  , asyncSyncActionExC
  ) where

-- for docs
import Game.GoreAndAsh

import Game.GoreAndAsh.Async.API as X 
import Game.GoreAndAsh.Async.Module as X 
import Game.GoreAndAsh.Async.State as X 