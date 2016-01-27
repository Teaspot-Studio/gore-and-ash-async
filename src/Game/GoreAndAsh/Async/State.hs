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
  , AsyncId(..)
  , ValueMap
  , emptyAsyncState
  , registerAsyncValue
  , getFinishedAsyncValue
  ) where

import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Data.Dynamic 
import Data.Hashable
import GHC.Generics (Generic)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H 

-- | Id of async value, it is used to poll info about the value
newtype AsyncId = AsyncId { unAsyncId :: Int }
  deriving (Generic, Eq, Show)

instance Hashable AsyncId 
instance NFData AsyncId 

-- | Container of async values
type ValueMap = HashMap AsyncId (Either (Async Dynamic) (Either SomeException Dynamic))

-- | Internal state of asynchronious core module
--
-- [@s@] - state of next module, they are chained until bottom, that is usually
--         an empty data type.
data AsyncState s = AsyncState {
  asyncValues :: !ValueMap 
, asyncNextId :: !Int
, asyncNextState :: !s
} deriving (Generic)

instance NFData s => NFData (AsyncState s) where 
  rnf AsyncState{..} = 
    asyncValues `seq`
    asyncNextState `deepseq`
    ()

-- | Create inital async state
--
-- [@s@] -  state of next module
emptyAsyncState :: s -> AsyncState s 
emptyAsyncState s = AsyncState {
    asyncValues = H.empty
  , asyncNextId = 0
  , asyncNextState = s
  }

-- | Put async value into internal state
registerAsyncValue :: Typeable a => Async a -> AsyncState s -> (AsyncId, AsyncState s)
registerAsyncValue av s = (i, s {
    asyncNextId = asyncNextId s + 1
  , asyncValues = H.insert i (Left $ toDyn <$> av) . asyncValues $! s
  })
  where
    i = AsyncId . asyncNextId $! s

-- | Try to get value of given async value
--
-- Note: first 'Maybe' layer is test for existense of given async value
-- 
-- Note: second 'Maybe' layer is test is the value is finished 
getFinishedAsyncValue :: AsyncId -> AsyncState s -> Maybe (Maybe (Either SomeException Dynamic))
getFinishedAsyncValue i AsyncState{..} = check <$> H.lookup i asyncValues 
  where 
  check v = case v of 
    Left _ -> Nothing
    Right a -> Just a