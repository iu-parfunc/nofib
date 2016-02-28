{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (forM_, when)
import Data.List (isPrefixOf)

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
  files <- getDirectoryContents dir
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
