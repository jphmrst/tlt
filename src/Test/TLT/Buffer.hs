{-|
Module      : Buffer
Description : Testing in a monad transformer layer
Copyright   : (c) John Maraist, 2022
License     : GPL3
Maintainer  : haskell-tlt@maraist.org
Stability   : experimental
Portability : POSIX

Buffer for accumulating test results in the @TLT@ testing system.  See
`Test.TLT` for more information.

-}

module Test.TLT.Buffer where
import Test.TLT.Results

-- |Accumulator for test results, in the style of a simplified Huet's
-- zipper which only ever adds to the end of the structure.
--
-- Defined in module `Test.TLT.Buffer`.
data TRBuf = Buf TRBuf Int Int String [TestResult] | Top Int Int [TestResult]

-- |Add a single test result to a `TRBuf`.
--
-- Defined in module `Test.TLT.Buffer`.
addResult :: TRBuf -> TestResult -> TRBuf
addResult (Top tc fc trs) tr =
  Top (tc + testCount tr) (fc + failCount tr) $ tr : trs
addResult (Buf up tc fc s trs) tr =
  Buf up (tc + testCount tr) (fc + failCount tr) s $ tr : trs

-- |Convert the topmost group of a bottom-up `TRBuf` into a completed
-- top-down report about the group.
--
-- Defined in module `Test.TLT.Buffer`.
currentGroup :: TRBuf -> TestResult
currentGroup (Buf up tc fc s trs) = Group s tc fc (reverse trs)

-- |Derive a new `TRBuf` corresponding to finishing the current group
-- and continuing to accumulate results into its enclosure.
--
-- Defined in module `Test.TLT.Buffer`.
popGroup :: TRBuf -> TRBuf
popGroup trb@(Buf acc _ _ _ _) = addResult acc $ currentGroup trb

-- |Convert a `TRBuf` into a list of top-down `TestResult`s.
--
-- Defined in module `Test.TLT.Buffer`.
closeTRBuf :: TRBuf -> [TestResult]
closeTRBuf (Top _ _ ts) = reverse ts
closeTRBuf b = closeTRBuf $ popGroup b
