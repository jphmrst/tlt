{-|
Module      : Standard
Description : Standard test operations for TLT
Copyright   : (c) John Maraist, 2022
License     : GPL3
Maintainer  : haskell-tlt@maraist.org
Stability   : experimental
Portability : POSIX

Standard assertion vocabulary for the @TLT@ testing system.  See
`Test.TLT` for more information.

-}

module Test.TLT.Standard where
import Data.Maybe
import Test.TLT.Assertion
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

infix 1 @==,  @/=,  @<,  @>,  @<=,  @>=
infix 1 @==-, @/=-, @<-, @>-, @<=-, @>=-

-- |Assert that two values are equal.  This assertion takes an
-- expected and an actual /value/; see `(@==)` to compare the result
-- of a /monadic computation/ to an expected value.
--
-- ===== Examples
--
-- > test :: MonadIO m => TLT m ()
-- > test = do
-- >   "Make sure that 2 is still equal to itself" ~: 2 @==- 2
-- >   "Make sure that there are four lights" ~: 4 @==- length lights
(@==-) :: (MonadIO m, Eq a, Show a) => a -> a -> Assertion m
(@==-) = liftAssertion2Pure (==) $
  \ exp actual -> "Expected " ++ show exp ++ " but got " ++ show actual

-- |Assert that a calculated value is as expected.  This assertion
-- compare the result of a /monadic computation/ to an expected value;
-- see `(@==-)` to compare an /actual value/ to the expected value.
--
-- ===== Examples
--
-- > test :: MonadIO m => TLT m ()
-- > test = do
-- >   "Make sure that 2 is still equal to itself" ~: 2 @== return 2
-- >   "Make sure that there are four lights" ~: 4 @== countLights
-- >                                             -- where countLights :: m Int
(@==) :: (MonadIO m, Eq a, Show a) => a -> m a -> Assertion m
(@==) = assertion2PtoM (@==-)

-- |Assert that two values are not equal.  This assertion takes an
-- expected and an actual /value/; see `(@/=)` to compare the result
-- of a /monadic computation/ to an expected value.
(@/=-) :: (MonadIO m, Eq a, Show a) => a -> a -> Assertion m
(@/=-) = liftAssertion2Pure (/=) $
  \ exp actual ->
    "Expected other than " ++ show exp ++ " but got " ++ show actual

-- |Assert that a calculated value differs from some known value.
-- This assertion compares the result of a /monadic computation/ to an
-- expected value; see `(@/=-)` to compare an /actual value/ to the
-- expected value.
(@/=) :: (MonadIO m, Eq a, Show a) => a -> m a -> Assertion m
(@/=) = assertion2PtoM (@/=-)

-- |Assert that a given boundary is strictly less than some value.
-- This assertion takes an expected and an actual /value/; see `(@<)`
-- to compare the result of a /monadic computation/ to an expected
-- value.
(@<-) :: (MonadIO m, Ord a, Show a) => a -> a -> Assertion m
(@<-) = liftAssertion2Pure (<) $
  \ exp actual ->
    "Lower bound (open) is " ++ show exp ++ " but got " ++ show actual

-- |Assert that a given, constant boundary is strictly less than some
-- calculated value.  This assertion compares the result of a /monadic
-- computation/ to an expected value; see `(@<-)` to compare an
-- /actual value/ to the expected value.
(@<) :: (MonadIO m, Ord a, Show a) => a -> m a -> Assertion m
(@<) = assertion2PtoM (@<-)

-- |Assert that a given boundary is strictly less than some value.
-- This assertion takes an expected and an actual /value/; see `(@>)`
-- to compare the result of a /monadic computation/ to an expected
-- value.
(@>-) :: (MonadIO m, Ord a, Show a) => a -> a -> Assertion m
(@>-) = liftAssertion2Pure (>) $
  \ exp actual ->
    "Upper bound (open) is " ++ show exp ++ " but got " ++ show actual

-- |Assert that a given, constant boundary is strictly less than some
-- calculated value.  This assertion compares the result of a /monadic
-- computation/ to an expected value; see `(@>-)` to compare an
-- /actual value/ to the expected value.
(@>) :: (MonadIO m, Ord a, Show a) => a -> m a -> Assertion m
(@>) = assertion2PtoM (@>-)

-- |Assert that a given boundary is strictly less than some value.
-- This assertion takes an expected and an actual /value/; see `(@<=)`
-- to compare the result of a /monadic computation/ to an expected
-- value.
(@<=-) :: (MonadIO m, Ord a, Show a) => a -> a -> Assertion m
(@<=-) = liftAssertion2Pure (<=) $
  \ exp actual ->
    "Lower bound (closed) is " ++ show exp ++ " but got " ++ show actual

-- |Assert that a given, constant boundary is strictly less than some
-- calculated value.  This assertion compares the result of a /monadic
-- computation/ to an expected value; see `(@<=-)` to compare an
-- /actual value/ to the expected value.
(@<=) :: (MonadIO m, Ord a, Show a) => a -> m a -> Assertion m
(@<=) = assertion2PtoM (@<=-)

-- |Assert that a given boundary is strictly less than some value.
-- This assertion takes an expected and an actual /value/; see `(@>=)`
-- to compare the result of a /monadic computation/ to an expected
-- value.
(@>=-) :: (MonadIO m, Ord a, Show a) => a -> a -> Assertion m
(@>=-) = liftAssertion2Pure (>=) $
  \ exp actual ->
    "Upper bound (closed) is " ++ show exp ++ " but got " ++ show actual

-- |Assert that a given, constant boundary is strictly less than some
-- calculated value.  This assertion compares the result of a /monadic
-- computation/ to an expected value; see `(@>=-)` to compare an
-- /actual value/ to the expected value.
(@>=) :: (MonadIO m, Ord a, Show a) => a -> m a -> Assertion m
(@>=) = assertion2PtoM (@>=-)

-- |Assert that a pure traversable structure (such as a list) is
-- empty.
emptyP :: (MonadIO m, Traversable t) => t a -> Assertion m
emptyP = liftAssertionPure null
           (\ _ -> "Expected empty structure but got non-empty")

-- |Assert that a traversable structure (such as a list) returned from
-- a computation is empty.
empty :: (MonadIO m, Traversable t) => m (t a) -> Assertion m
empty = assertionPtoM emptyP

-- |Assert that a pure traversable structure (such as a list) is
-- nonempty.
nonemptyP :: (MonadIO m, Traversable t) => t a -> Assertion m
nonemptyP = liftAssertionPure (not . null)
              (\ _ -> "Expected non-empty structure but got empty")

-- |Assert that a traversable structure (such as a list) returned from
-- a computation is non-empty.
nonempty :: (MonadIO m, Traversable t) => m (t a) -> Assertion m
nonempty = assertionPtoM nonemptyP

-- |Assert that a `Maybe` value is `Nothing`.
nothingP :: MonadIO m => Maybe a -> Assertion m
nothingP = liftAssertionPure isNothing
           (\ _ -> "Expected empty Maybe value but got non-Nothing")

-- |Assert that a `Maybe` result of a computation is `Nothing`.
nothing :: MonadIO m => m (Maybe a) -> Assertion m
nothing = assertionPtoM nothingP
