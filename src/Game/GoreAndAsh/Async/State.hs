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
  ) where

import Control.Concurrent.Async
import Control.DeepSeq
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
type ValueMap = HashMap AsyncId (Async Dynamic)

-- | Internal state of asynchronious core module
--
-- [@s@] - state of next module, they are chained until bottom, that is usually
--         an empty data type.
data AsyncState s = AsyncState {
  asyncValues :: !ValueMap 
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
  , asyncNextState = s
  }