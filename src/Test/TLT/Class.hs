{-|
Module      : Class
Description : Testing in a monad transformer layer
Copyright   : (c) John Maraist, 2022
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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
  tltStateAccum :: TRBuf
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
runTLT :: TLTReady m => TLT m r -> m (TLTopts, [TestResult])
runTLT (TLT t) = do
  (_, state) <- runStateT t $ TLTstate {
    tltStateOptions = defaultOpts,
    tltStateAccum = Top 0 0 []
    }
  return (tltStateOptions state, closeTRBuf $ tltStateAccum state)

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
inGroup :: MonadIO m => String -> TLT m a -> TLT m a
inGroup name group = do
  state <- TLT get
  TLT $ put $
    state { tltStateAccum = Buf (tltStateAccum state) 0 0 name [] }
  result <- group
  state' <- TLT $ get
  TLT $ put $
    state' { tltStateAccum = popGroup $ tltStateAccum state' }
  return result

{- --------------------------------------------------------------- -}

-- | Class of monads which support TLT testings.
class MonadIO m => TLTReady m where
  -- | Run the `IO`-based computation of one test.
  runForTest :: m [TestFail] -> IO [TestFail]

-- | An `IO` computation is the base case.
instance TLTReady IO where runForTest = id

-- | Inductive cases run the top layer, and recursively call
-- `runForTest` on the next layer.
instance TLTReady m => TLTReady (IdentityT m) where
  runForTest = runForTest . runIdentityT

-- | The exception case from an `ExceptT` run translates to a
-- `TestFail` entry of an error..
instance (Show e, TLTReady m) => TLTReady (ExceptT e m) where
  runForTest = runForTest . fmap (\x -> case x of
                                     Left l -> [Erred $ show l]
                                     Right r -> r) . runExceptT

-- | We take the `Nothing` case of a `MaybeT` as an ansence of
-- failure.  Experience may show this to be wrong.
instance TLTReady m => TLTReady (MaybeT m) where
  runForTest = runForTest . fmap (\x -> case x of
                                     Nothing -> []
                                     Just x -> x) . runMaybeT

{-
instance TLTReady m => TLTReady (ReaderT r m) where
  liftTLTExcept = lift . liftTLTExcept

instance TLTReady m => TLTReady (ResourceT m) where
  liftTLTExcept = lift . liftTLTExcept

instance TLTReady m =>
         TLTReady (SL.StateT s m) where
  liftTLTExcept = lift . liftTLTExcept

instance TLTReady m => TLTReady (STT s m) where
  liftTLTExcept = lift . liftTLTExcept
  runToExcept = runToExcept . runSTT

-- | The `runToExcept` function in this case simply discards any
-- output.
instance (TLTReady m, Monoid w) =>
         TLTReady (WL.WriterT w m) where
  liftTLTExcept = lift . liftTLTExcept
  runToExcept m = runToExcept $ do (res, _) <- WL.runWriterT m
                                   return res

-- | The `runToExcept` function in this case simply discards any
-- output.
instance (TLTReady m, Monoid w) =>
         TLTReady (WS.WriterT w m) where
  liftTLTExcept = lift . liftTLTExcept
  runToExcept m = runToExcept $ do (res, _) <- WS.runWriterT m
                                   return res
-}

{- ---------------------------------------------------------------

-- | Enabling TLT checking of the completion of computations with- or
-- without uncaught exceptions in a (possibly embedded) `ExceptT` or
-- `Except` monad.
--
-- In general, it is more difficult to automatically deduce
-- @MonadTLTExcept@ instances than @MonadTLT@ because `runToExcept`
-- instances bodies will frequently require additional parameters to
-- functions such as `runReaderT`, or values corresponding to
-- `Nothing`, which are specific to a particular scenario.
--
-- Note that using @MonadTLTExcept@ imposes the restriction that the
-- `TLT` transformer layer must be wrapped within the `ExceptT`
-- transformer layer.
class (MonadTLT m nt, Monad m, MonadTLT ne nt) => MonadTLTExcept m e nt ne
      | m -> e, m -> ne where
  -- | Encodes how an embedded `ExceptT` monad can be lifted to the
  -- top-level monad stack type @m@.
  liftTLTExcept :: ExceptT e ne a -> m a
  -- | Runs the layers of the monad stack above the `ExceptT` layer,
  -- exposing that latter layer.  Serves as an inverse of
  -- `liftTLTExcept`.
  runToExcept :: m a -> ExceptT e ne a

-- | The `ExceptT` instance is a base case; here the lift/run
-- functions are simply `id`.
instance MonadTLT m nt => MonadTLTExcept (ExceptT e m) e nt m where
  liftTLTExcept = id
  runToExcept = id

{-
-- I don't understand the FreeT transformer well enough to build this.
instance (MonadTLTExcept m e nt ne, Functor f) =>
         MonadTLTExcept (FreeT f m) e nt ne where
  liftTLTExcept = lift . liftTLTExcept
  runToExcept = ???
-}

-- | We can infer general instances for other monad transformer types
-- when their @run@ function does not take some initializing argument.
instance MonadTLTExcept m e nt ne => MonadTLTExcept (IdentityT m) e nt ne where
  liftTLTExcept = lift . liftTLTExcept
  runToExcept = runToExcept . runIdentityT

{-
instance (MonadTLTExcept m e nt ne) =>
         MonadTLTExcept (MaybeT m) e nt ne where
  liftTLTExcept = lift . liftTLTExcept
  runToExcept m = runToExcept $ do res <- runMaybeT m
                                   case res of
                                     Nothing -> return "???"
                                     Just j -> return j

instance MonadTLTExcept m e nt ne => MonadTLTExcept (ReaderT r m) e nt ne where
  liftTLTExcept = lift . liftTLTExcept

instance MonadTLTExcept m e nt ne => MonadTLTExcept (ResourceT m) e nt ne where
  liftTLTExcept = lift . liftTLTExcept

instance MonadTLTExcept m e nt ne =>
         MonadTLTExcept (SL.StateT s m) e nt ne where
  liftTLTExcept = lift . liftTLTExcept

instance MonadTLTExcept m e nt ne => MonadTLTExcept (STT s m) e nt ne where
  liftTLTExcept = lift . liftTLTExcept
  runToExcept = runToExcept . runSTT
-}

-- | The `runToExcept` function in this case simply discards any
-- output.
instance (MonadTLTExcept m e nt ne, Monoid w) =>
         MonadTLTExcept (WL.WriterT w m) e nt ne where
  liftTLTExcept = lift . liftTLTExcept
  runToExcept m = runToExcept $ do (res, _) <- WL.runWriterT m
                                   return res

-- | The `runToExcept` function in this case simply discards any
-- output.
instance (MonadTLTExcept m e nt ne, Monoid w) =>
         MonadTLTExcept (WS.WriterT w m) e nt ne where
  liftTLTExcept = lift . liftTLTExcept
  runToExcept m = runToExcept $ do (res, _) <- WS.runWriterT m
                                   return res

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
