{-|
Module      : Results
Description : Results representation for TLT
Copyright   : (c) John Maraist, 2022
License     : GPL3
Maintainer  : haskell-tlt@maraist.org
Stability   : experimental
Portability : POSIX

Results representation for the @TLT@ testing system.  See `Test.TLT`
for more information.

-}

module Test.TLT.Results where

-- * Results of tests

-- |Reasons why a test might fail.
--
-- Defined in module `Test.TLT.Results`.
data TestFail = Asserted String
                -- ^ A failure arising from an `Test.TLT.Assertion`
                -- which is not met.
              | Erred String
                -- ^ A failure associated with a call to a Haskell
                -- function triggering an error.

-- |Default conversion of a `TestFail` to a descriptive string.
--
-- Defined in module `Test.TLT.Results`.
formatFail :: TestFail -> String
formatFail (Asserted s) = s
formatFail (Erred s) = "Assertion raised exception: " ++ s

-- |Hierarchical structure holding the result of running tests,
-- possibly grouped into tests.
--
-- Defined in module `Test.TLT.Results`.
data TestResult = Test String [TestFail]
                | Group String Int Int [TestResult]
                  -- ^ The `Int`s are respectively the total number of
                  -- tests executed, and total number of failures
                  -- detected.

-- |Return the number of failed tests reported in a `TestResult`.
--
-- Defined in module `Test.TLT.Results`.
failCount :: TestResult -> Int
failCount (Test _ []) = 0
failCount (Test _ _) = 1
failCount (Group _ _ n _) = n

-- |Return the number of tests described by a `TestResult`.
--
-- Defined in module `Test.TLT.Results`.
testCount :: TestResult -> Int
testCount (Test _ _) = 1
testCount (Group _ n _ _) = n

-- |Return the number of failed tests described in a list of
-- `TestResult`s.
--
-- Defined in module `Test.TLT.Results`.
totalFailCount :: [TestResult] -> Int
totalFailCount = foldr (+) 0 . map failCount

-- |Return the number of tests described in a list of `TestResult`s.
--
-- Defined in module `Test.TLT.Results`.
totalTestCount :: [TestResult] -> Int
totalTestCount = foldr (+) 0 . map testCount
