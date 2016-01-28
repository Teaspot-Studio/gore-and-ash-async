module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.DeepSeq
import Control.Monad.Catch
import Control.Monad.Fix 
import Control.Monad.IO.Class
import Data.Typeable 
import GHC.Generics (Generic)

import Control.Wire
import Game.GoreAndAsh.Core
import Game.GoreAndAsh.Async 

-- | Application monad is monad stack build from given list of modules over base monad (IO or Identity)
type AppStack = ModuleStack [AsyncT, AsyncT] IO
newtype AppState = AppState (ModuleState AppStack)
  deriving (Generic)

instance NFData AppState 

-- | Wrapper around type family
newtype AppMonad a = AppMonad (AppStack a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadThrow, MonadCatch, MonadAsync)

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

main :: IO ()
main = withModule (Proxy :: Proxy AppMonad) $ do
  defaultMainWithOpts [
      testGroup "async actions" [
        testCase "simple" asyncSimple
      ]
    , testGroup "async bound actions" [
      ]
    , testGroup "async sync actions" [
      ]
    ] mempty

-- | Runs wire n times and return it result
runWire :: Int -> AppWire () a -> IO (Maybe a)
runWire n w = do 
  gs <- newGameState w
  go gs n Nothing
  where 
  go gs i ma = if i <= 0 then return ma
    else do 
      (ma', gs') <- stepGame gs (return ())
      go gs' (i-1) ma'

asyncSimple :: Assertion
asyncSimple = do 
  ma <- runWire 100 w 
  assertEqual "wire switch" (Just True) ma
  where
    w = proc _ -> do 
      e <- asyncAction (return True) -< ()
      rSwitch (pure False) -< ((), pure <$> e)