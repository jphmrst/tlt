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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.TLT.Class where

import Control.Exception
import Control.Monad
import Control.Monad.ST.Trans
import Control.Monad.Trans.Class
-- import Control.Monad.Trans.Either
import Control.Monad.Trans.Free
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State.Strict
import qualified Control.Monad.Trans.State.Lazy as SL
import qualified Control.Monad.Trans.Writer.Lazy as WL
import qualified Control.Monad.Trans.Writer.Strict as WS
import Test.TLT.Options
import Test.TLT.Results
import Test.TLT.Buffer

-- |Synonym for the elements of the `TLT` state.
--
-- Defined in module `Test.TLT.Class`.
type TLTstate = (TLTopts, TRBuf)

-- |Monad transformer for TLT tests.  This layer stores the results
-- from tests as they are executed.
--
-- Defined in module `Test.TLT.Class`.
newtype Monad m => TLT m r = TLT { unwrap :: StateT TLTstate m r }
  deriving (Functor, Applicative, Monad, MonadTrans)

-- |Extending `TLT` operations across other monad transformers.  For
-- easiest and most flexible testing, declare the monad transformers
-- of your application as instances of this class.
--
-- Defined in module `Test.TLT.Class`.
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

instance MonadTLT m n => MonadTLT (SL.StateT s m) n where
  liftTLT = lift . liftTLT

instance MonadTLT m n => MonadTLT (STT s m) n where
  liftTLT = lift . liftTLT

instance (MonadTLT m n, Monoid w) => MonadTLT (WL.WriterT w m) n where
  liftTLT = lift . liftTLT

instance (MonadTLT m n, Monoid w) => MonadTLT (WS.WriterT w m) n where
  liftTLT = lift . liftTLT

-- |Execute the tests specified in a `TLT` monad without output
-- side-effects, returning the final options and result reports.
--
-- This function is primarily useful when calling TLT from some other
-- package.  If you are using TLT itself as your test framework, and
-- wishing to see its human-oriented output directly, consider using
-- `Test.TLT.tlt` instead.
--
-- Defined in module `Test.TLT.Class`.
tltCore :: Monad m => TLT m r -> m (TLTopts, [TestResult])
tltCore (TLT t) = do
  (_, (opts, resultsBuf)) <- runStateT t $ (defaultOpts, Top 0 0 [])
  return (opts, closeTRBuf resultsBuf)

-- |This function controls whether `Test.TLT.tlt` will report only
-- tests which fail, suppressing any display of tests which pass, or
-- else report the results of all tests.  The default is the former:
-- the idea is that no news should be good news, with the programmer
-- bothered only with problems which need fixing.
--
-- Defined in module `Test.TLT.Class`.
reportAllTestResults :: MonadTLT m n => Bool -> m ()
reportAllTestResults b = liftTLT $ TLT $ do
  (opts, tr) <- get
  put $ (opts `withShowPasses` b, tr)

-- |This function controls whether the main `Test.TLT.tlt` executable
-- should exit after displaying test results which include at least
-- one failing test.  By default, it will exit in this situation.  The
-- idea is that a test suite can be broken into parts when it makes
-- sense to run the latter parts only when the former parts all pass.
--
-- Defined in module `Test.TLT.Class`.
setExitAfterFailDisplay :: MonadTLT m n => Bool -> m ()
setExitAfterFailDisplay b = liftTLT $ TLT $ do
  (opts, tr) <- get
  put $ (opts `withExitAfterFail` b, tr)

-- |Report a failure.  Useful in pattern-matching cases which are
-- entirely not expected.
--
-- Defined in module `Test.TLT.Class`.
tltFail :: MonadTLT m n => String -> String -> m ()
desc `tltFail` detail = liftTLT $ TLT $ do
  (opts, before) <- get
  let after = addResult before $ Test desc [Asserted detail]
  put (opts, after)

-- |Organize the tests in the given subcomputation as a separate group
-- within the test results we will report.
--
-- Defined in module `Test.TLT.Class`.
inGroup :: MonadTLT m n => String -> m a -> m a
inGroup name group = do
  (opts, before) <- liftTLT $ TLT get
  liftTLT $ TLT $ put $ (opts, Buf before 0 0 name [])
  result <- group
  (opts', after) <- liftTLT $ TLT $ get
  liftTLT $ TLT $ put $ (opts', popGroup after)
  return result
