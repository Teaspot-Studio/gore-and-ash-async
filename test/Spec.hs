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

import Control.Concurrent.MVar
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Either
import Game.GoreAndAsh.Async 
import Game.GoreAndAsh.Core
import Prelude hiding (id, (.))

import qualified Data.Sequence as S 

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
      , testCase "except no catch" asyncExcept'
      , testCase "cancel" asyncCancel
      , testCase "cancel delayed" asyncCancelDelayed
      , testCase "factory" asyncFactory
      ]
    , testGroup "async bound actions" [
        testCase "simple" asyncBoundSimple
      , testCase "except" asyncBoundExcept
      , testCase "except no catch" asyncBoundExcept'
      , testCase "cancel" asyncCancelBound
      , testCase "cancel delayed" asyncCancelBoundDelayed
      , testCase "factory" asyncFactoryBound
      ]
    , testGroup "async sync actions" [
        testCase "simple" asyncSyncSimple
      , testCase "except" asyncSyncExcept
      , testCase "except no catch" asyncSyncExcept'
      , testCase "cancel" asyncCancelSync
      , testCase "factory" asyncFactorySync
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

asyncCancel :: Assertion
asyncCancel = do 
  ma <- runWire 100 w 
  assertEqual "wire switch" (Just False) ma
  where
    w = proc _ -> do 
      ce <- now -< ()
      e <- asyncActionC (return True) -< ce
      rSwitch (pure False) -< ((), pure <$> e)

asyncCancelDelayed :: Assertion
asyncCancelDelayed = do 
  ma <- runWire 100 w0 
  assertEqual "wire switch" (Just False) ma
  where
    w0 = switch $ proc _ -> do 
      evar <- liftGameMonadEvent1 (const $ liftIO newEmptyMVar) . now -< ()
      returnA -< (False, w <$> evar)

    w var = proc _ -> do 
      ce <- delay NoEvent . now -< ()
      e <- asyncActionC (io var) -< ce
      liftGameMonadEvent1 (const . liftIO $ putMVar var ()) -< ce
      rSwitch (pure False) -< ((), pure <$> e)

    io var = do 
      _ <- readMVar var
      return True

asyncFactory :: Assertion 
asyncFactory = do 
  ma <- runWire 100 w
  assertEqual "summed values" (Just 20) ma
  where
  w = proc _ -> do 
    eio <- wgen -< ()
    eas <- asyncActionFactory -< eio
    hold . accumE (\i as -> i + sum as) 0 -< eas 

  wgen :: AppWire () (Event (S.Seq (IO Int))) 
  wgen = go 2 10
    where
    go k 0 = never
    go k n = mkSFN $ \_ -> (Event $ S.replicate k (return 1), go k (n-1))

asyncExcept' :: Assertion
asyncExcept' = do 
  ma <- (fmap Right $ runWire 100 w) `catchAll` (return . Left)
  assertBool "returned is exception" (isLeft ma) 
  where 
    w :: AppWire () Bool
    w = proc _ -> do 
      e <- asyncAction (throwM TestException) -< ()
      rSwitch (pure False) -< ((), pure <$> e)

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

asyncBoundExcept' :: Assertion
asyncBoundExcept' = do 
  ma <- (fmap Right $ runWire 100 w) `catchAll` (return . Left)
  assertBool "returned is exception" (isLeft ma) 
  where 
    w :: AppWire () Bool
    w = proc _ -> do 
      e <- asyncActionBound (throwM TestException) -< ()
      rSwitch (pure False) -< ((), pure <$> e)

asyncCancelBound :: Assertion
asyncCancelBound = do 
  ma <- runWire 100 w 
  assertEqual "wire switch" (Just False) ma
  where
    w = proc _ -> do 
      ce <- now -< ()
      e <- asyncActionBoundC (return True) -< ce
      rSwitch (pure False) -< ((), pure <$> e)

asyncCancelBoundDelayed :: Assertion
asyncCancelBoundDelayed = do 
  ma <- runWire 100 w0 
  assertEqual "wire switch" (Just False) ma
  where
    w0 = switch $ proc _ -> do 
      evar <- liftGameMonadEvent1 (const $ liftIO newEmptyMVar) . now -< ()
      returnA -< (False, w <$> evar)

    w var = proc _ -> do 
      ce <- delay NoEvent . now -< ()
      e <- asyncActionBoundC (io var) -< ce
      liftGameMonadEvent1 (const . liftIO $ putMVar var ()) -< ce
      rSwitch (pure False) -< ((), pure <$> e)

    io var = do 
      _ <- readMVar var
      return True

asyncFactoryBound :: Assertion 
asyncFactoryBound = do 
  ma <- runWire 100 w
  assertEqual "summed values" (Just 20) ma
  where
  w = proc _ -> do 
    eio <- wgen -< ()
    eas <- asyncActionBoundFactory -< eio
    hold . accumE (\i as -> i + sum as) 0 -< eas 

  wgen :: AppWire () (Event (S.Seq (IO Int))) 
  wgen = go 2 10
    where
    go k 0 = never
    go k n = mkSFN $ \_ -> (Event $ S.replicate k (return 1), go k (n-1))

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

asyncSyncExcept' :: Assertion
asyncSyncExcept' = do 
  ma <- (fmap Right $ runWire 100 w) `catchAll` (return . Left)
  assertBool "returned is exception" (isLeft ma) 
  where 
    w :: AppWire () Bool
    w = proc _ -> do 
      e <- asyncSyncAction (throwM TestException) -< ()
      rSwitch (pure False) -< ((), pure <$> e)

asyncCancelSync :: Assertion
asyncCancelSync = do 
  ma <- runWire 100 w 
  assertEqual "wire switch" (Just False) ma
  where
    w = proc _ -> do 
      ce <- now -< ()
      e <- asyncSyncActionC (return True) -< ce
      rSwitch (pure False) -< ((), pure <$> e)

asyncFactorySync :: Assertion 
asyncFactorySync = do 
  ma <- runWire 100 w
  assertEqual "summed values" (Just 20) ma
  where
  w = proc _ -> do 
    eio <- wgen -< ()
    eas <- asyncSyncActionFactory -< eio
    hold . accumE (\i as -> i + sum as) 0 -< eas 

  wgen :: AppWire () (Event (S.Seq (IO Int))) 
  wgen = go 2 10
    where
    go k 0 = never
    go k n = mkSFN $ \_ -> (Event $ S.replicate k (return 1), go k (n-1))