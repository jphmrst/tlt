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

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.TLT.Report where
import Control.Monad
import Control.Monad.IO.Class
import System.Console.ANSI
import System.Exit
import Test.TLT.Options
import Test.TLT.Results

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

boldBlack = setSGR [
  SetColor Foreground Vivid Black, SetConsoleIntensity BoldIntensity ]
boldRed = setSGR [
  SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity ]
boldGreen = setSGR [
  SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity ]

mediumRed = setSGR [
  SetColor Foreground Vivid Red, SetConsoleIntensity NormalIntensity ]
mediumGreen = setSGR [
  SetColor Foreground Vivid Green, SetConsoleIntensity NormalIntensity ]
mediumBlue = setSGR [
  SetColor Foreground Vivid Blue, SetConsoleIntensity NormalIntensity ]
mediumBlack = setSGR [
  SetColor Foreground Vivid Black, SetConsoleIntensity NormalIntensity ]

greenPass = do
  mediumBlue
  putStr "Pass"
  mediumBlack

redFail = do
  boldRed
  putStr "FAIL"
  mediumBlack
