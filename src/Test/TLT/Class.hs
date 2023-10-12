{-|
Module      : Class
Description : Testing in a monad transformer layer
Copyright   : (c) John Maraist 2022, 2023
License     : GPL3
Maintainer  : haskell-tlt@maraist.org
Stability   : experimental
Portability : POSIX

Main state and monad definitions for the @TLT@ testing system.  See
`Test.TLT` for more information.

-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.TLT.Class where

import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.ST.Trans
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
-- import Control.Monad.Trans.Either
import Control.Monad.Trans.Except
import Control.Monad.Trans.Free
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State.Strict
import qualified Control.Monad.Trans.State.Lazy as SL
import qualified Control.Monad.Trans.Writer.Lazy as WL
import qualified Control.Monad.Trans.Writer.Strict as WS
import Data.Kind (Type)
import Test.TLT.Options
import Test.TLT.Results
import Test.TLT.Buffer

-- |Synonym for the elements of the `TLT` state.
data TLTstate (m :: Type -> Type) = TLTstate {
  tltStateOptions :: TLTopts,
  tltStateAccum :: TRBuf,
  tltStateTestAssertionWrapper :: TLT m [TestFail] -> TLTstate m -> TLT m [TestFail],
  tltStateGroupRunWrapper :: forall a . TLT m a -> TLT m a
  }

-- |Monad transformer for TLT tests.  This layer stores the results
-- from tests as they are executed.
newtype TLT (m :: Type -> Type) r = TLT { unwrap :: StateT (TLTstate m) m r }
  deriving (Functor, Applicative, Monad, MonadIO)

-- Not able to autoderive this one.
instance MonadTrans TLT where lift = TLT . lift

{- ----------------------------------------------------------------- -}

-- |Execute the tests specified in a `TLT` monad without output
-- side-effects, returning the final options and result reports.
--
-- This function is primarily useful when calling TLT from some other
-- package.  If you are using TLT itself as your test framework, and
-- wishing to see its human-oriented output directly, consider using
-- `Test.TLT.tlt` instead.
runTLT :: (Testable m, MonadIO m) => TLT m r -> m (TLTopts, [TestResult])
runTLT (TLT t) = do
  (_, state) <- runStateT t $ TLTstate {
    tltStateOptions = defaultOpts,
    tltStateAccum = Top 0 0 [],
    tltStateTestAssertionWrapper = testRunWithCatch,
    tltStateGroupRunWrapper = id
    }
  return (tltStateOptions state, closeTRBuf $ tltStateAccum state)

-- | Runner for a single test catching all thrown exceptions.
testRunWithCatch ::
  (Testable m, MonadIO m) => TLT m [TestFail] -> TLTstate m -> TLT m [TestFail]
testRunWithCatch a state =
  TLT $ liftIO $ catch (do r <- runForTest $ runTLTtest state a
                           return $! r)
                     (\e -> return [Erred $ show (e :: SomeException)])

-- | Prepare to execute one test in the monad wrapped for tests.
runTLTtest :: MonadIO m => TLTstate m -> TLT m [TestFail] -> m [TestFail]
runTLTtest s (TLT t) = do
  (tfs, _) <- runStateT t s
  return tfs

-- |This function controls whether `Test.TLT.tlt` will report only
-- tests which fail, suppressing any display of tests which pass, or
-- else report the results of all tests.  The default is the former:
-- the idea is that no news should be good news, with the programmer
-- bothered only with problems which need fixing.
reportAllTestResults :: MonadIO m => Bool -> TLT m ()
reportAllTestResults b = TLT $ do
  state <- get
  put $ state { tltStateOptions = tltStateOptions state `withShowPasses` b }

-- |This function controls whether the main `Test.TLT.tlt` executable
-- should exit after displaying test results which include at least
-- one failing test.  By default, it will exit in this situation.  The
-- idea is that a test suite can be broken into parts when it makes
-- sense to run the latter parts only when the former parts all pass.
setExitAfterFailDisplay :: MonadIO m => Bool -> TLT m ()
setExitAfterFailDisplay b = TLT $ do
  state <- get
  put $ state { tltStateOptions = tltStateOptions state `withExitAfterFail` b }

-- |Report a failure.  Useful in pattern-matching cases which are
-- entirely not expected.
tltFail :: MonadIO m => String -> String -> TLT m ()
desc `tltFail` detail = TLT $ do
  state <- get
  let after = addResult (tltStateAccum state) $ Test desc [Asserted detail]
  put $ state { tltStateAccum = after }

-- |Report a success.  Useful in default cases.
tltPass :: MonadIO m => String -> TLT m ()
tltPass desc = TLT $ do
  state <- get
  let after = addResult (tltStateAccum state) $ Test desc []
  put $ state { tltStateAccum = after }

-- |Organize the tests in the given subcomputation as a separate group
-- within the test results we will report.
inGroup :: forall m a . MonadIO m => String -> TLT m a -> TLT m a
inGroup name group = do
  state <- TLT get
  TLT $ put $
    state { tltStateAccum = Buf (tltStateAccum state) 0 0 name [] }
  result <- tltStateGroupRunWrapper state group
  state' <- TLT $ get
  TLT $ put $
    state' { tltStateAccum = popGroup $ tltStateAccum state' }
  return result

{- --------------------------------------------------------------- -}

{-
-- | Class of monad transformers which preserve TLT testability modulo
-- universal quantification over a dummy type variable, for example
-- `STT`.
class TestableT2 m where
  run2TransForTest :: Monad n =>
    forall a . (forall s . m s n [TestFail]) -> n [TestFail]

instance TestableT2 STT where run2TransForTest = runSTT

class Testable2 m where
  -- | Run the `IO`-based computation of one test.
  run2ForTest :: (forall s . m s [TestFail]) -> IO [TestFail]

instance (Testable m, TestableT2 t, Monad m) => Testable2 (t m) where
  run2ForTest = runForTest . run2TransForTest
-}

-- | Class of monad transformers which preserve TLT testability.
--
-- Some standard transformers do not have built-in instances:
--
--  - `Control.Monad.Trans.Reader.runReaderT`,
--    `Control.Monad.Trans.State.Lazy.runStateT`, and
--    `Control.Monad.Trans.State.Strict.runStateT` all require an
--    argument of the initial internal state type;
--
--  - `runSTT` requires its argument to be universally quantified over
--    the state thread dummy type, which we do not have in this class
--    signature;
--
--  - The `ResourceT` transformer imposes an additional constraint on
--  - the underlying monad which is not expressible here (but notice
--  - the instance declaration on `Testable` `ResourceT`).
class TestableT m where
  runTransForTest :: forall n . Monad n => m n [TestFail] -> n [TestFail]

instance TestableT IdentityT where runTransForTest = runIdentityT

instance Show e => TestableT (ExceptT e) where
  runTransForTest = fmap (\x -> case x of
                                  Left l -> [Erred $ show l]
                                  Right r -> r) . runExceptT

instance TestableT MaybeT where
  runTransForTest = fmap (\x -> case x of
                                  Nothing -> []
                                  Just x -> x) . runMaybeT

instance Monoid w => TestableT (WL.WriterT w) where
  runTransForTest = fmap (\(a,_) -> a) . WL.runWriterT

instance Monoid w => TestableT (WS.WriterT w) where
  runTransForTest = fmap (\(a,_) -> a) . WS.runWriterT

-- | Class of monads which support TLT testings.
--
-- Some standard classes do not have built-in instances:
-- `Control.Monad.Trans.Reader.runReaderT`,
-- `Control.Monad.Trans.State.Lazy.runStateT`, and
-- `Control.Monad.Trans.State.Strict.runStateT` all require an
-- argument of the initial internal state type; `runSTT` requires its
-- argument to be universally quantified over the state thread dummy
-- type, which we do not have in this class signature.
class Testable m where
  -- | Run the `IO`-based computation of one test.
  runForTest :: m [TestFail] -> IO [TestFail]

-- | An `IO` computation is the base case.
instance Testable IO where runForTest = id

instance (Testable m, TestableT t, Monad m) => Testable (t m) where
  runForTest = runForTest . runTransForTest

instance (Testable m, MonadUnliftIO m) => Testable (ResourceT m) where
  runForTest = runForTest . runResourceT

{- ---------------------------------------------------------------

-- | Ensure that a computation in `ExceptT` completes without an
-- uncaught exception.
noUncaught_ :: MonadTLTExcept m e nt ne => String -> m a -> m ()
noUncaught_ loc m = do
  let label = "No uncaught exception from " ++ loc
  liftTLTExcept $ catchE (do runToExcept m
                             tltPass label)
                         (\_ -> label `tltFail` "Uncaught exception")

-- | Ensure that a computation in `ExceptT` completes without an
-- uncaught exception.
noUncaught :: (MonadTLTExcept m e nt ne, Show e) => String -> m a -> m ()
noUncaught loc m = do
  let label = "No uncaught exception from " ++ loc
  liftTLTExcept $ catchE (do runToExcept m
                             tltPass label)
                         (\ex -> label `tltFail`
                                   ("Uncaught exception: " ++ show ex))

-- | Ensure that a computation in `ExceptT` does throw an uncaught
-- exception, allowing further testing of the exception.
uncaughtWith ::
  (MonadTLTExcept m e nt ne) => String -> m a -> (e -> ExceptT e ne ()) -> m ()
uncaughtWith loc m handler =
  liftTLTExcept $ catchE (do runToExcept m
                             ("Expected uncaught exception from " ++ loc)
                               `tltFail` "Did not throw exception")
                         handler

-- | Ensure that a computation in `ExceptT` does throw an uncaught
-- exception.
uncaught loc m = uncaughtWith loc m $ \_ -> return ()
---------------------------------------------------------------------- -}
