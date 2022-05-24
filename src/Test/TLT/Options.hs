{-|
Module      : Options
Description : Option spec for TLT
Copyright   : (c) John Maraist, 2022
License     : GPL3
Maintainer  : haskell-tlt@maraist.org
Stability   : experimental
Portability : POSIX

Options representation for the @TLT@ testing system.  See `Test.TLT`
for more information.

-}

module Test.TLT.Options where

-- |Record of options which may be specified for running and reporting
-- TLT tests.
data TLTopts = TLTopts {
  optShowPasses :: Bool,
  optQuitAfterFailReport :: Bool
}

-- |Default initial options.
--
-- Defined in `Test.TLT.Options`.
defaultOpts = TLTopts False True

-- |Update the display of showing passes in a `TLTopts` record.
--
-- Defined in `Test.TLT.Options`.
withShowPasses :: TLTopts -> Bool -> TLTopts
withShowPasses (TLTopts _ f) b = TLTopts b f

-- |Update the display of showing passes in a `TLTopts` record.
--
-- Defined in `Test.TLT.Options`.
withExitAfterFail :: TLTopts -> Bool -> TLTopts
withExitAfterFail (TLTopts p _) b = TLTopts p b
