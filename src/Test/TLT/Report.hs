{-|
Module      : Report
Description : Testing in a monad transformer layer
Copyright   : (c) John Maraist 2022, 2023
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
          then do boldIntense
                  redText
                  putStrLn $
                    "Found " ++ show fails ++ " error"
                      ++ (if fails > 1 then "s" else "")
                      ++ " in " ++ show tests ++ " tests; exiting"
                  mediumIntense
                  blackText
                  mediumIntense
                  blackText
                  when exitAfterFailDisplay exitFailure
          else do boldIntense
                  greenText
                  putStrLn $ show tests ++ " test"
                    ++ (if tests > 1 then "s" else "")
                    ++ " passing."
                  mediumIntense
                  blackText
                  mediumIntense
                  blackText
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

-- |Command to set an ANSI terminal to medium-weight.
mediumIntense :: IO ()
mediumIntense = setSGR [ SetConsoleIntensity NormalIntensity ]

-- |Command to set an ANSI terminal to bold-weight.
boldIntense :: IO ()
boldIntense = setSGR [ SetConsoleIntensity BoldIntensity ]

-- |Command to set an ANSI terminal to a black foreground.
blackText :: IO ()
blackText = setSGR [ SetColor Foreground Vivid Black ]

-- |Command to set an ANSI terminal to a red foreground.
redText :: IO ()
redText = setSGR [ SetColor Foreground Vivid Red ]

-- |Command to set an ANSI terminal to a blue foreground.
blueText :: IO ()
blueText = setSGR [ SetColor Foreground Vivid Blue ]

-- |Command to set an ANSI terminal to a green foreground.
greenText :: IO ()
greenText = setSGR [ SetColor Foreground Vivid Green ]

-- |Command to set an ANSI terminal to the standard TLT weight and
-- color for a passing test.
greenPass = do
  mediumIntense
  blueText
  putStr "Pass"
  mediumIntense
  blackText

-- |Command to set an ANSI terminal to the standard TLT weight and
-- color for a failing test.
redFail = do
  boldIntense
  redText
  putStr "FAIL"
  mediumIntense
  blackText
