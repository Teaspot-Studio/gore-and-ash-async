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
  ) where

import Control.DeepSeq
import GHC.Generics (Generic)

-- | Internal state of asynchronious core module
--
-- [@s@] - state of next module, they are chained until bottom, that is usually
--         an empty data type.
data AsyncState s = AsyncState {
  asyncNextState :: !s
} deriving (Generic)

instance NFData s => NFData (AsyncState s)

-- | Create inital async state
--
-- [@s@] -  state of next module
emptyAsyncState :: s -> AsyncState s 
emptyAsyncState s = AsyncState {
    asyncNextState = s
  }