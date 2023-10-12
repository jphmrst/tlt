{-|
Module      : Assertion
Description : Assertions and related utilities for TLT
Copyright   : (c) John Maraist 2022, 2023
License     : GPL3
Maintainer  : haskell-tlt@maraist.org
Stability   : experimental
Portability : POSIX

Assertion infrastructure for the @TLT@ testing system.  See `Test.TLT`
for more information.

-}

{-# LANGUAGE ScopedTypeVariables #-}

module Test.TLT.Assertion where

import Control.Exception
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Test.TLT.Results
import Test.TLT.Buffer
import Test.TLT.Class

-- * Specifying individual tests

-- |An assertion is a computation (typically in the monad wrapped by
-- `TLT`) which returns a list of zero of more reasons for the failure
-- of the assertion.  A successful computation returns an empty list:
-- no reasons for failure, hence success.
type Assertion m = m [TestFail]

-- |This assertion always fails with the given message.
assertFailed :: MonadIO m => String -> Assertion m
assertFailed msg = return [Asserted msg]

-- |This assertion always succeeds.
assertSuccess :: MonadIO m => Assertion m
assertSuccess = return []

infix 0 ~:, ~::, ~::-

-- |Label and perform a test of an `Assertion`.
--
-- ===== Example
--
-- > test :: MonadIO m => TLT m ()
-- > test = do
-- >   "2 is 2 as result" ~: 2 @== return 2    -- This test passes.
-- >   "2 not 3" ~: 2 @/=- 3                   -- This test fails.
(~:) :: (Testable m, MonadIO m) => String -> Assertion (TLT m) -> TLT m ()
s ~: a = do
  state <- TLT get
  let wrapper = tltStateTestAssertionWrapper state
  assessment <- wrapper a state
  TLT $ put $
    state { tltStateAccum =
              addResult (tltStateAccum state) $
                Test s assessment }

-- |Label and perform a test of a (pure) boolean value.
--
-- ===== Example
--
-- > test :: MonadIO m => TLT m ()
-- > test = do
-- >   "True passes" ~::- return True                 -- This test passes.
-- >   "2 is 2 as single Bool" ~::- return (2 == 2)   -- This test passes.
-- >   "2 is 3!?" ~::- myFn 4 "Hammer"                -- Passes if myFn (which
-- >                                                  -- must be monadic)
-- >                                                  -- returns True.
(~::-) :: (Testable m, MonadIO m) => String -> Bool -> TLT m ()
s ~::- b = s ~:
  return (if b then [] else [Asserted $ "Expected True but got False"])
{-# INLINE (~::-) #-}

-- |Label and perform a test of a boolean value returned by a
-- computation in the wrapped monad @m@.
--
-- ===== Example
--
-- > test :: MonadIO m => TLT m ()
-- > test = do
-- >   "True passes" ~::- True               -- This test passes.
-- >   "2 is 2 as single Bool" ~::- 2 == 2   -- This test passes.
-- >   "2 is 3!?" ~::- 2 == 2                -- This test fails.
(~::) :: (Testable m, MonadIO m) => String -> TLT m Bool -> TLT m ()
s ~:: bM =
  s ~: fmap (\b -> if b
                   then []
                   else [Asserted $ "Expected True but got False"])
            bM
{-# INLINE (~::) #-}

-- |Transform a binary function on an expected and an actual value
-- (plus a binary generator of a failure message) into an `Assertion`
-- for a pure given actual value.
--
-- ===== Example
--
-- TLT's scalar-testing operators like @\@==-@ are defined with this
-- function:
--
-- > (@==-) :: (MonadIO m, Eq a, Show a) => a -> a -> Assertion m
-- > (@==-) = liftAssertion2Pure (==) $
-- >   \ exp actual -> "Expected " ++ show exp ++ " but got " ++ show actual
--
-- The `(==)` operator tests equality, and the result here allows the
-- assertion that a value should be exactly equal to a target.  The
-- second argument formats the detail reported when the assertion
-- fails.
liftAssertion2Pure ::
  MonadIO m => (a -> a -> Bool) -> (a -> a -> String) -> a -> a -> Assertion m
liftAssertion2Pure tester explainer exp actual = return $
  if (tester exp actual) then [] else [Asserted $ explainer exp actual]

-- |Given an `Assertion` for two pure values (expected and actual),
-- lift it to an `Assertion` expecting the actual value to be returned
-- from a computation.
--
-- ===== Examples
--
-- The TLT assertion `Test.TLT.(@==)` lifts `Test.TLT.(@==-)` (both
-- defined in `Test.TLT.Standard`) from expecting a pure actual result
-- to expecting a computation returning a value to test.
--
-- > (@==) :: (MonadIO m, Eq a, Show a) => a -> m a -> Assertion m
-- > (@==) = assertion2PtoM (@==-)
assertion2PtoM ::
  MonadIO m => (a -> a -> Assertion m) -> a -> m a -> Assertion m
assertion2PtoM pa exp actualM = do actual <- actualM
                                   pa exp actual

-- |Transform a binary function on expected and actual values (plus
-- a generator of a failure message) into an `Assertion` where the
-- actual value is to be returned from a subcomputation.
liftAssertion2M ::
  MonadIO m => (a -> a -> Bool) -> (a -> a -> String) -> a -> m a -> Assertion m
liftAssertion2M tester explainer exp actualM =
  let assertPure = liftAssertion2Pure tester explainer exp
  in do actual <- actualM
        assertPure actual

-- |Transform a unary function on a value (plus a generator of a
-- failure message) into a unary function returning an `Assertion` for
-- a pure given actual value.
--
-- ===== Example
--
-- The TLT assertion `Test.TLT.emptyP` (defined in
-- `Test.TLT.Standard`) is built from the `Traversable` predicate
-- `null`
--
-- > emptyP :: (MonadIO m, Traversable t) => t a -> Assertion m
-- > emptyP = liftAssertionPure null
-- >            (\ _ -> "Expected empty structure but got non-empty")

liftAssertionPure ::
  MonadIO m => (a -> Bool) -> (a -> String) -> a -> Assertion m
liftAssertionPure tester explainer actual = return $
  if (tester actual) then [] else [Asserted $ explainer actual]

-- |Given an `Assertion` for a pure (actual) value, lift it to an
-- `Assertion` expecting the value to be returned from a computation.
--
-- ===== Example
--
-- The TLT assertion `Test.TLT.empty` (defined in `Test.TLT.Standard`)
-- on monadic computations returning lists is defined in terms of the
-- corresponging assertion on pure list-valued expressions.
--
-- > empty :: (MonadIO m, Traversable t) => m (t a) -> Assertion m
-- > empty = assertionPtoM emptyP
assertionPtoM :: MonadIO m => (a -> Assertion m) -> m a -> Assertion m
assertionPtoM pa actualM = do actual <- actualM
                              pa actual

-- |Transform a unary function on an actual value (plus a generator of
-- a failure message) into an `Assertion` where the value is to be
-- returned from a subcomputation.
liftAssertionM ::
  MonadIO m => (a -> Bool) -> (a -> String) -> m a -> Assertion m
liftAssertionM tester explainer actualM =
  let assertPure = liftAssertionPure tester explainer
  in do actual <- actualM
        assertPure actual
