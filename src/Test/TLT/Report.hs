{-|
Module      : Report
Description : Testing in a monad transformer layer
Copyright   : (c) John Maraist, 2022
License     : GPL3
Maintainer  : haskell-tlt@maraist.org
Stability   : experimental
Portability : POSIX

Default results reporting for the @TLT@ testing system.  See
`Test.TLT` for more information.

-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.TLT.Report where
import Data.Kind (Type)
import Control.Monad
import Control.Monad.IO.Class
import System.Console.ANSI
import System.Exit
import Test.TLT.Options
import Test.TLT.Results
import Test.TLT.Class

-- |Execute the tests specified in a `TLT` monad, and report the
-- results as text output.
--
-- When using TLT from some other package (as opposed to using TLT
-- itself as your test framework, and wishing to see its
-- human-oriented output directly), consider using `runTLT` instead.
tlt :: TLTReady m => TLT m r -> m ()
tlt tlt = do
  liftIO $ putStrLn "Running tests:"
  (opts, results) <- runTLT tlt
  liftIO $ report opts results

-- |Report the results of tests.
report :: TLTopts -> [TestResult] -> IO ()
report (TLTopts showPasses exitAfterFailDisplay) trs =
  let fails = totalFailCount trs
      tests = totalTestCount trs
  in do report' "" trs
        if fails > 0
          then do boldRed
                  putStrLn $
                    "Found " ++ show fails ++ " error"
                      ++ (if fails > 1 then "s" else "")
                      ++ " in " ++ show tests ++ " tests; exiting"
                  mediumBlack
                  when exitAfterFailDisplay exitFailure
          else do boldGreen
                  putStrLn $ show tests ++ " test"
                    ++ (if tests > 1 then "s" else "")
                    ++ " passing."
                  mediumBlack
  where report' ind trs = forM_ trs $ \ tr ->
          when (failCount tr > 0 || showPasses) $
            case tr of
              Test s r -> do
                putStr $ ind ++ "- " ++ s ++ ": "
                case r of
                  [] -> do
                    greenPass
                    putStrLn ""
                  x : [] -> do
                    redFail
                    putStrLn $ " " ++ formatFail x
                  _ -> do
                    redFail
                    putStrLn ":"
                    forM_ r $ \ f -> putStrLn $ ind ++ "- " ++ formatFail f
              Group s _ _ trs' -> do
                putStrLn $ ind ++ "- " ++ s ++ ":"
                report' ("  " ++ ind) trs'

-- |Command to set an ANSI terminal to boldface black.
boldBlack = setSGR [
  SetColor Foreground Vivid Black, SetConsoleIntensity BoldIntensity ]
-- |Command to set an ANSI terminal to boldface red.
boldRed = setSGR [
  SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity ]
-- |Command to set an ANSI terminal to boldface green.
boldGreen = setSGR [
  SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity ]

-- |Command to set an ANSI terminal to medium-weight red.
mediumRed = setSGR [
  SetColor Foreground Vivid Red, SetConsoleIntensity NormalIntensity ]
-- |Command to set an ANSI terminal to medium-weight green.
mediumGreen = setSGR [
  SetColor Foreground Vivid Green, SetConsoleIntensity NormalIntensity ]
-- |Command to set an ANSI terminal to medium-weight blue.
mediumBlue = setSGR [
  SetColor Foreground Vivid Blue, SetConsoleIntensity NormalIntensity ]
-- |Command to set an ANSI terminal to medium-weight black.
mediumBlack = setSGR [
  SetColor Foreground Vivid Black, SetConsoleIntensity NormalIntensity ]

-- |Command to set an ANSI terminal to the standard TLT weight and
-- color for a passing test.
greenPass = do
  mediumBlue
  putStr "Pass"
  mediumBlack

-- |Command to set an ANSI terminal to the standard TLT weight and
-- color for a failing test.
redFail = do
  boldRed
  putStr "FAIL"
  mediumBlack
