{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (forM_, when)
import Data.List (isPrefixOf, isSubsequenceOf, sort)

import Debug.Trace

import System.Directory (getDirectoryContents)
import System.Environment (getArgs, getEnv)
import System.FilePath ((</>), dropExtension)
import System.Process (readProcess)

compFileName :: FilePath -> FilePath -> FilePath
compFileName f1 f2 = 
  let f1' = dropExtension $ drop 3 f1
      f2' = dropExtension $ drop 3 f2
  in "comp" ++ f1' ++ f2' ++ ".lgfile"

piecewiseAnalyse :: FilePath -> IO ()
piecewiseAnalyse dir = do
  let f filename = filename /= "."
                && filename /= ".."
                && "log-" `isPrefixOf` filename
                && not ("magramal" `isSubsequenceOf` filename)
                   -- ^ Let's prune out the results so that we only
                   -- use the logs from one machine. Otherwise, this
                   -- will take forever.
  files <- (sort . filter f) <$> getDirectoryContents dir
  forM_ files traceShowM
  forM_ files $ \f1 -> forM_ files $ \f2 ->
    when (f1 /= f2) $ do
       stdout <- readProcess "nofib-analyse/nofib-analyse"
                             [dir </> f1, dir </> f2] ""
       writeFile (traceShowId $ dir </> compFileName f1 f2) stdout

main :: IO ()
main = do
  getArgs >>= \case
                (d:_) -> piecewiseAnalyse d
                _     -> putStrLn "usage: piecewise-analyse <dir>"
