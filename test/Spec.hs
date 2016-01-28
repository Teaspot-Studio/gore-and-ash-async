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
import Data.Either
import Game.GoreAndAsh.Async 
import Game.GoreAndAsh.Core

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

data TestException = TestException deriving Show 

instance Exception TestException

main :: IO ()
main = withModule (Proxy :: Proxy AppMonad) $ do
  defaultMainWithOpts [
      testGroup "async actions" [
        testCase "simple" asyncSimple
      , testCase "except" asyncExcept
      ]
    , testGroup "async bound actions" [
        testCase "simple" asyncBoundSimple
      , testCase "except" asyncBoundExcept
      ]
    , testGroup "async sync actions" [
        testCase "simple" asyncSyncSimple
      , testCase "except" asyncSyncExcept
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

asyncExcept :: Assertion
asyncExcept = do 
  ma <- runWire 100 w 
  assertBool "returned is exception" (check ma) 
  where 
    check ma = case ma of 
      Nothing -> False 
      Just ea -> isLeft ea 

    w :: AppWire () (Either SomeException Bool)
    w = proc _ -> do 
      e <- asyncActionEx (throwM TestException) -< ()
      rSwitch (pure $ Right False) -< ((), pure <$> e)

asyncBoundSimple :: Assertion
asyncBoundSimple = do 
  ma <- runWire 100 w 
  assertEqual "wire switch" (Just True) ma
  where
    w = proc _ -> do 
      e <- asyncActionBound (return True) -< ()
      rSwitch (pure False) -< ((), pure <$> e)

asyncBoundExcept :: Assertion
asyncBoundExcept = do 
  ma <- runWire 100 w 
  assertBool "returned is exception" (check ma) 
  where 
    check ma = case ma of 
      Nothing -> False 
      Just ea -> isLeft ea 

    w :: AppWire () (Either SomeException Bool)
    w = proc _ -> do 
      e <- asyncActionBoundEx (throwM TestException) -< ()
      rSwitch (pure $ Right False) -< ((), pure <$> e)

asyncSyncSimple :: Assertion
asyncSyncSimple = do 
  ma <- runWire 100 w 
  assertEqual "wire switch" (Just True) ma
  where
    w = proc _ -> do 
      e <- asyncSyncAction (return True) -< ()
      rSwitch (pure False) -< ((), pure <$> e)

asyncSyncExcept :: Assertion
asyncSyncExcept = do 
  ma <- runWire 100 w 
  assertBool "returned is exception" (check ma) 
  where 
    check ma = case ma of 
      Nothing -> False 
      Just ea -> isLeft ea 

    w :: AppWire () (Either SomeException Bool)
    w = proc _ -> do 
      e <- asyncSyncActionEx (throwM TestException) -< ()
      rSwitch (pure $ Right False) -< ((), pure <$> e)