{-|
Module      : TLT
Description : Testing in a monad transformer layer
Copyright   : (c) John Maraist, 2022
License     : GPL3
Maintainer  : haskell-tlt@maraist.org
Stability   : experimental
Portability : POSIX

TLT is a small unit test system oriented towards examining
intermediate results of computations in monad transformers.  It is
intended to be lightweight for the programmer, and does not require
tests to be specified in some sort of formal list of tests.  Rather,
tests are simply commands in a monad stack which includes the
transformer layer @Test.TLT@.

-}

module Test.TLT (
  -- * The TLT transformer
  TLT, tlt, MonadTLT, liftTLT, tltCore,
  -- ** Session options
  reportAllTestResults, setExitAfterFailDisplay,
  -- * Writing tests
  Assertion,
  -- ** `TLT` commands
  (~:), (~::), (~::-), tltFail, inGroup,
  -- ** Assertions
  -- *** About the values of pure expressions of `Eq`- and `Ord`-type
  (@==),  (@/=),  (@<),  (@>),  (@<=),  (@>=),
  -- *** About monadic computations returing `Eq`s and `Ord`s
  (@==-), (@/=-), (@<-), (@>-), (@<=-), (@>=-),
  -- *** About list values
  empty, nonempty, emptyP, nonemptyP,
  -- *** About `Maybe` values
  nothing, nothingP, assertFailed, assertSuccess,
  -- ** Building new assertions
  -- *** Unary assertions
  liftAssertionPure, assertionPtoM, liftAssertionM,
  -- *** Binary assertions
  liftAssertion2Pure, assertion2PtoM, liftAssertion2M

  ) where

-- This package does not actually define any functions; it merely
-- re-exports from the internal packages.

import Test.TLT.Options
import Test.TLT.Results
import Test.TLT.Buffer
import Test.TLT.Class
import Test.TLT.Report
import Test.TLT.Assertion
import Test.TLT.Standard
