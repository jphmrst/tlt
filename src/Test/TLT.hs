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

This module is a re-exporter for the various @Test.TLT.*@
modules which define distinct portions of the TLT system.  These
exports are oriented towards the simple use of TLT as a test
framework.  When using TLT more programmatically, such as when
integrating TLT into another test framework, it may be necessary
to import the more internally-oriented functions of the
individual modules.

-}

module Test.TLT (

  -- * Introduction and basic use

  -- | The basic use of TLT is a call to @tlt@ in the @main@ function
  -- of a program, followed by a monadic computation which asserts
  -- various properties about the results it calculates.  For example:
  --
  -- > main :: IO ()
  -- > main = tlt $ runExceptT test
  -- >   "True passes" ~::- True
  -- >   "1 and 1 make 2" ~: 2 @== return (1 + 1)

  tlt,

  -- ** Writing tests

  -- | The two tests in the example above have the common form of one
  -- individual TLT test:
  --
  -- >  LABLE TEST-OPERATOR EXPRESSION
  --
  -- There are three @TEST-OPERATOR@s, corresponding to three
  -- different forms of @EXPRESSION@:
  --
  -- +----------+----------------------------------------------------+
  -- | OPERATOR | EXPRESSION                                         |
  -- +==========+====================================================+
  -- | `~:`     | The expression is an assertion written with one of |
  -- |          | the several operators below.                       |
  -- +----------+----------------------------------------------------+
  -- | `~::`    | The expression is a monadic computation returning  |
  -- |          | a boolean value, where @True@ corresponds to the   |
  -- |          | test passing.                                      |
  -- +----------+----------------------------------------------------+
  -- | `~::-`   | The expression is a simple boolean value, where    |
  -- |          | again @True@ corresponds to the test passing.      |
  -- +----------+----------------------------------------------------+
  --
  -- The last two of these test-introducung operations show a pattern
  -- that recrus throughout TLT: where two operators differ only where
  -- one has a trailing hyphen, the version __without__ the hyphen
  -- refers to a monadic computation, and the version __with__ the
  -- hyphen refers to a pure expression.

  -- | There are a number of special forms of test, and commands for
  -- setting session options.
  --
  --    * The `tltFail` function introduces a test which always fails.
  --      This function is useful in pattern matches, for wholly
  --      unacceptable combinations.
  --
  --    * The `reportAllTestResults` function controls whether TLT
  --      (when invoked with `tlt` as described above) should display
  --      only tests which fails, or should display all passing tests
  --      as well.  The former is the default, since the latter can be
  --      quite verbose.
  --
  --    * The `setExitAfterFailDisplay` function directs `tlt` to exit
  --      after displaying a set of test results which include at
  --      least one failing test. The idea of this default is that a
  --      test suite can be broken into parts when it makes sense to
  --      run the latter parts only when the former parts all pass.
  --
  --    * The `inGroup` function groups several tests together as a
  --      single group.  The `tlt` function displays the tests of a
  --      group indented, which helps to visually group related tests
  --      together.
  --
  -- All of these test and option forms are formally documented below.

  -- ** Writing standard assertions

  -- | There are a number of pre-defined forms of assertion imported
  -- automatically from @Test.TLT@.  Note that more operators are in
  -- pairs, with one comparison for monadic results, and one for pure
  -- values.
  --
  -- +-------------------------+---------------------------------------+
  -- | `@==`, `@==-`           | Asserts equality of two `Eq` values.  |
  -- +-------------------------+---------------------------------------+
  -- | `@/=`, `@/=-`           | Asserts inequality of two `Eq`        |
  -- |                         | values.                               |
  -- +-------------------------+---------------------------------------+
  -- | `@<`, `@<-`             | Asserts a less-than relation between  |
  -- |                         | two `Ord` values.                     |
  -- +-------------------------+---------------------------------------+
  -- | `@>`, `@>-`             | Asserts a greater-than relation       |
  -- |                         | between two`Ord` values.              |
  -- +-------------------------+---------------------------------------+
  -- | `@<=`, `@<=-`           | Asserts a less-than-or-equal-to       |
  -- |                         | relation between two `Ord` values.    |
  -- +-------------------------+---------------------------------------+
  -- | `@>=`, `@>=-`           | Asserts a greater-than-or-equal-to    |
  -- |                         | relation between two `Ord` values.    |
  -- +-------------------------+---------------------------------------+
  -- | `empty`, `emptyP`       | Asserts the emptiness of a            |
  -- |                         | traversable structure.                |
  -- +-------------------------+---------------------------------------+
  -- | `nonempty`, `nonemptyP` | Asserts the non-emptiness of a        |
  -- |                         | traversable structure.                |
  -- +-------------------------+---------------------------------------+
  -- + `nothing`, `nothingP`   | Asserts that a `Maybe` value is       |
  -- |                         | `Nothing`.                            |
  -- +-------------------------+---------------------------------------+
  --
  -- The predefined operators, along with functions for defining new
  -- `Assertion` operators, are documented more formally below.

  -- ** Dealing with exceptions

  -- | TLT's interaction with exceptions thrown from the `Except`
  -- monad or from an `ExceptT` transformer layer is subtle.  Because
  -- TLT does not have a specification of tests separate from the
  -- tests' execution, TLT will notice test failures only it actually
  -- runs them.  Tests which may be viewed by the human programmer as
  -- implicitly failing because a thrown exception prevented them from
  -- running are __not__ recorded or reported as failures.  TLT
  -- provides three functions for checking for thrown exceptions.  The
  -- first argument of each is a @TLT@ monad of tests which has been
  -- declared to take an `ExceptT` layer.
  --
  --  [`noUncaught` and `noUncaught_`]: Both assert that /no/ uncaught
  --  exceptions should be thrown from its argument computation, and
  --  fails if one is.  The `noUncaught_` function accepts any type of
  --  exception, but cannot report any details about them except that
  --  /something/ was thrown.  The `noUncaught` function demands that
  --  the exception type be of class `Show`, and does report exception
  --  details in the case of failure.
  --
  --  [`uncaught`]: Asserts that an uncaught exception /should/ be
  --  thrown from its argument computation, and fails if none are
  --  thrown.
  --
  --  [`uncaughtWith`]: Asserts that an uncaught exception /should/ be
  --  thrown from its argument computation, fails if none are thrown,
  --  and passes the thrown exception to its second argument for
  --  further inspection.
  --
  -- The declaration that a monadic value includes an `ExceptT` layer
  -- to be checked is made by declaring instances of the
  -- `MonadTLTExcept` class.  The use of these exception-checking
  -- functions requires that the `TLT` transformer layer be contained
  -- within the `ExceptT` layer.  The generated documentation for this
  -- class and its predefined instances, as well as the above
  -- functions, are all below.

  -- * The TLT transformer
  TLT, -- MonadTLT(liftTLT),
  -- ** Session options
  reportAllTestResults, setExitAfterFailDisplay,

  -- * Tests
  (~:), (~::), (~::-), tltFail, inGroup,

  -- * Assertions
  Assertion,
  -- *** About the values of pure expressions of `Eq`- and `Ord`-type
  (@==-), (@/=-), (@<-), (@>-), (@<=-), (@>=-),
  -- *** About monadic computations returing `Eq`s and `Ord`s
  (@==),  (@/=),  (@<),  (@>),  (@<=),  (@>=),
  -- *** About list values
  empty, nonempty, emptyP, nonemptyP,
  -- *** About `Maybe` values
  nothing, nothingP,
  -- *** Unconditional assertions
  assertFailed, assertSuccess,

  -- ** Building new assertions
  -- *** Unary assertions
  liftAssertionPure, assertionPtoM, liftAssertionM,
  -- *** Binary assertions
  liftAssertion2Pure, assertion2PtoM, liftAssertion2M,
  -- -- * Dealing with exceptions
  -- MonadTLTExcept(liftTLTExcept, runToExcept),
  -- noUncaught, noUncaught_, uncaught, uncaughtWith

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
import Control.Monad.Trans.Except -- For Haddock links
