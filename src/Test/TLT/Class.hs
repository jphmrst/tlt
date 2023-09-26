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

{-# LANGUAGE FlexibleInstances #-}
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

type Interceptor m = m [TestFail] -> m [TestFail]

-- | Call prior to a series of TLT tests to detect general errors.
-- Requires that the underlying computation be `MonadIO`.
interceptNothing :: Monad m => m [TestFail] -> m [TestFail]
interceptNothing = id

-- |Synonym for the elements of the `TLT` state.
data TLTstate (m :: Type -> Type) = TLTstate {
  tltStateOptions :: TLTopts,
  tltStateAccum :: TRBuf,
  tltInterceptor :: Interceptor m
  }

-- |Monad transformer for TLT tests.  This layer stores the results
-- from tests as they are executed.
newtype {- Monad m => -} TLT (m :: Type -> Type) r =
  TLT { unwrap :: StateT (TLTstate m) m r }
    deriving (Functor, Applicative, Monad)

-- Not able to autoderive this one.
instance MonadTrans TLT where lift = TLT . lift

-- |Extending `TLT` operations across other monad transformers.  For
-- easiest and most flexible testing, declare the monad transformers
-- of your application as instances of this class.
class (Monad m, Monad n) => MonadTLT m n | m -> n where
  -- |Lift TLT operations within a monad transformer stack.  Note that
  -- with enough transformer types included in this class, the
  -- @liftTLT@ function should usually be unnecessary: the commands in
  -- this module which actually configure testing, or specify a test,
  -- already @liftTLT@ their own result.  So they will all act as
  -- top-level transformers in @MonadTLT@.
  liftTLT :: TLT n a -> m a

instance Monad m => MonadTLT (TLT m) m where
  liftTLT = id

instance (MonadTLT m n, Functor f) => MonadTLT (FreeT f m) n where
    liftTLT = lift . liftTLT

instance MonadTLT m n => MonadTLT (IdentityT m) n where
  liftTLT = lift . liftTLT

instance MonadTLT m n => MonadTLT (MaybeT m) n where
  liftTLT = lift . liftTLT

instance MonadTLT m n => MonadTLT (ReaderT r m) n where
  liftTLT = lift . liftTLT

instance MonadTLT m n => MonadTLT (ResourceT m) n where
  liftTLT = lift . liftTLT

instance MonadTLT m n => MonadTLT (StateT s m) n where
  liftTLT = lift . liftTLT

instance MonadTLT m n => MonadTLT (ExceptT e m) n where
  liftTLT = lift . liftTLT

instance MonadTLT m n => MonadTLT (SL.StateT s m) n where
  liftTLT = lift . liftTLT

instance MonadTLT m n => MonadTLT (STT s m) n where
  liftTLT = lift . liftTLT

instance (MonadTLT m n, Monoid w) => MonadTLT (WL.WriterT w m) n where
  liftTLT = lift . liftTLT

instance (MonadTLT m n, Monoid w) => MonadTLT (WS.WriterT w m) n where
  liftTLT = lift . liftTLT

{- ----------------------------------------------------------------- -}

-- |Execute the tests specified in a `TLT` monad without output
-- side-effects, returning the final options and result reports.
--
-- This function is primarily useful when calling TLT from some other
-- package.  If you are using TLT itself as your test framework, and
-- wishing to see its human-oriented output directly, consider using
-- `Test.TLT.tlt` instead.
runTLT :: Monad m => TLT m r -> m (TLTopts, [TestResult])
runTLT (TLT t) = do
  (_, state) <- runStateT t $ TLTstate {
    tltStateOptions = defaultOpts,
    tltStateAccum = Top 0 0 [],
    tltInterceptor = interceptNothing
    }
  return (tltStateOptions state, closeTRBuf $ tltStateAccum state)

-- |This function controls whether `Test.TLT.tlt` will report only
-- tests which fail, suppressing any display of tests which pass, or
-- else report the results of all tests.  The default is the former:
-- the idea is that no news should be good news, with the programmer
-- bothered only with problems which need fixing.
reportAllTestResults :: MonadTLT m n => Bool -> m ()
reportAllTestResults b = liftTLT $ TLT $ do
  state <- get
  put $ state { tltStateOptions = tltStateOptions state `withShowPasses` b }

-- |This function controls whether the main `Test.TLT.tlt` executable
-- should exit after displaying test results which include at least
-- one failing test.  By default, it will exit in this situation.  The
-- idea is that a test suite can be broken into parts when it makes
-- sense to run the latter parts only when the former parts all pass.
setExitAfterFailDisplay :: MonadTLT m n => Bool -> m ()
setExitAfterFailDisplay b = liftTLT $ TLT $ do
  state <- get
  put $ state { tltStateOptions = tltStateOptions state `withExitAfterFail` b }

-- |Report a failure.  Useful in pattern-matching cases which are
-- entirely not expected.
tltFail :: MonadTLT m n => String -> String -> m ()
desc `tltFail` detail = liftTLT $ TLT $ do
  state <- get
  let after = addResult (tltStateAccum state) $ Test desc [Asserted detail]
  put $ state { tltStateAccum = after }

-- |Report a success.  Useful in default cases.
tltPass :: MonadTLT m n => String -> m ()
tltPass desc = liftTLT $ TLT $ do
  state <- get
  let after = addResult (tltStateAccum state) $ Test desc []
  put $ state { tltStateAccum = after }

-- |Organize the tests in the given subcomputation as a separate group
-- within the test results we will report.
inGroup :: MonadTLT m n => String -> m a -> m a
inGroup name group = do
  state <- liftTLT $ TLT get
  liftTLT $ TLT $ put $
    state { tltStateAccum = Buf (tltStateAccum state) 0 0 name [] }
  result <- group
  state' <- liftTLT $ TLT $ get
  liftTLT $ TLT $ put $
    state' { tltStateAccum = popGroup $ tltStateAccum state' }
  return result

{- --------------------------------------------------------------- -}

-- | Call prior to a series of TLT tests to detect general errors.
-- Requires that the underlying computation be `MonadIO`.
withIOErrorsByTLT ::
  (MonadTLT m n, MonadIO n) => (n [TestFail] -> IO [TestFail]) -> m ()
withIOErrorsByTLT runner = liftTLT $ TLT $ do
  state <- get
  put $ state { tltInterceptor = interceptExceptions runner }

-- | Call prior to a series of TLT tests to detect general errors.
-- Requires that the underlying computation be `MonadIO`.
interceptExceptions ::
  (MonadIO m) => (m [TestFail] -> IO [TestFail]) -> Interceptor m
interceptExceptions runner a =
  liftIO $ catch (runner a) $
    \e -> return $ [Erred $ show $ (e :: SomeException)]

{- --------------------------------------------------------------- -}

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
