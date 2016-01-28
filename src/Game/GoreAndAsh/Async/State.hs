{-|
Module      : Game.GoreAndAsh.Async.State
Description : Internal state of core module
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.Async.State(
    AsyncState(..)
  , emptyAsyncState
  -- * Async helpers
  , AsyncId(..)
  , AsyncValueMap
  , registerAsyncValue
  , getFinishedAsyncValue
  , cancelAsyncValue
  , purgeAsyncs
  -- * Sync helpers
  , SyncId(..)
  , SyncSheduled
  , SyncFinished
  , registerSyncValue
  , getFinishedSyncValue
  , cancelSyncValue
  , purgeSyncs
  ) where

import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Data.Dynamic 
import Data.Either
import Data.Hashable
import GHC.Generics (Generic)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H 

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S 

-- | Id of async value, it is used to poll info about the value
newtype AsyncId = AsyncId { unAsyncId :: Int }
  deriving (Generic, Eq, Show)

instance Hashable AsyncId 
instance NFData AsyncId 

-- | Container for async values
type AsyncValueMap = HashMap AsyncId (Either (Async Dynamic) (Either SomeException Dynamic))

-- | Id of sync value, it is used to identify return value
newtype SyncId = SyncId { unSyncId :: Int }
  deriving (Generic, Eq, Ord, Show)

instance Hashable SyncId 
instance NFData SyncId 

-- | Container for scheduled sync values
type SyncSheduled = Seq (SyncId, IO Dynamic)

-- | Container for finished sync values
type SyncFinished = HashMap SyncId (Either SomeException Dynamic)

-- | Internal state of asynchronious core module
--
-- [@s@] - state of next module, they are chained until bottom, that is usually
--         an empty data type.
data AsyncState s = AsyncState {
  asyncAValues :: !AsyncValueMap
, asyncScheduled :: !SyncSheduled
, asyncSValues :: !SyncFinished
, asyncNextId :: !Int
, asyncNextState :: !s
} deriving (Generic)

instance NFData s => NFData (AsyncState s) where 
  rnf AsyncState{..} = 
    asyncAValues `seq`
    asyncSValues `seq`
    asyncNextState `deepseq`
    ()

-- | Create inital async state
--
-- [@s@] -  state of next module
emptyAsyncState :: s -> AsyncState s 
emptyAsyncState s = AsyncState {
    asyncAValues = H.empty
  , asyncScheduled = S.empty
  , asyncSValues = H.empty
  , asyncNextId = 0
  , asyncNextState = s
  }

-- | Put async value into internal state
registerAsyncValue :: Typeable a => Async a -> AsyncState s -> (AsyncId, AsyncState s)
registerAsyncValue !av !s = (i, s {
    asyncNextId = asyncNextId s + 1
  , asyncAValues = H.insert i (Left $ toDyn <$> av) . asyncAValues $! s
  })
  where
    i = AsyncId . asyncNextId $! s

-- | Try to get value of given async value
--
-- Note: first 'Maybe' layer is test for existense of given async value
-- 
-- Note: second 'Maybe' layer is test is the value is finished 
getFinishedAsyncValue :: AsyncId -> AsyncState s -> Maybe (Maybe (Either SomeException Dynamic))
getFinishedAsyncValue !i AsyncState{..} = check <$> H.lookup i asyncAValues 
  where 
  check v = case v of 
    Left _ -> Nothing
    Right a -> Just a

-- | Unregister given id and return stored async
cancelAsyncValue :: AsyncId -> AsyncState s -> (Maybe (Async Dynamic), AsyncState s)
cancelAsyncValue !i !s = (check =<< H.lookup i (asyncAValues s), s {
    asyncAValues = H.delete i . asyncAValues $! s
  })
  where
  check v = case v of 
    Left a -> Just a
    Right _ -> Nothing

-- | Deletes calculated values
purgeAsyncs :: AsyncState s -> AsyncState s 
purgeAsyncs !s = s {
    asyncAValues = H.filter isLeft . asyncAValues $! s
  }

-- | Put sync value into internal state
registerSyncValue :: Typeable a => IO a -> AsyncState s -> (SyncId, AsyncState s)
registerSyncValue !io !s = (i, s {
    asyncNextId = asyncNextId s + 1
  , asyncScheduled = asyncScheduled s |> (i, toDyn <$> io)
  })
  where
    i = SyncId . asyncNextId $! s

-- | Try to get value of given sync value
--
-- Note: first 'Maybe' layer is test for existense of given async value
getFinishedSyncValue :: SyncId -> AsyncState s -> Maybe (Either SomeException Dynamic)
getFinishedSyncValue !i AsyncState{..} = H.lookup i asyncSValues 

-- | Unregister given sheduled sync action
cancelSyncValue :: SyncId -> AsyncState s -> AsyncState s
cancelSyncValue !i !s = s {
    asyncScheduled = S.filter ((/= i).fst) . asyncScheduled $! s
  }

-- | Deletes calculated values
purgeSyncs :: AsyncState s -> AsyncState s 
purgeSyncs !s = s {
    asyncSValues = H.empty
  }