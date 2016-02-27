{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (forM_, when)
import Data.List (isPrefixOf)

import System.Directory (getDirectoryContents)
import System.Environment (getArgs, getEnv)
import System.FilePath ((</>))
import System.Process (readProcess)

compFileName :: String -> FilePath -> FilePath -> FilePath
compFileName oldPref f1 f2 = 
  let oldPrefLen = length oldPref
      newPref    = "nofib-comp-"
      f1'        = take oldPrefLen f1
      f2'        = take oldPrefLen f2
  in newPref ++ f1' ++ f2'

piecewiseAnalyse :: FilePath -> IO ()
piecewiseAnalyse dir = do
  jobName <- getEnv "JOB_NAME" -- Jenkins-defined environmental variable
  let pref = "nofib-log-"
  files   <- filter (isPrefixOf $ pref ++ jobName) <$> getDirectoryContents dir
  forM_ files $ \f1 -> forM_ files $ \f2 ->
    when (f1 /= f2) $ do
       stdout <- readProcess "nofib-analyse/nofib-analyse"
                             [dir </> f1, dir </> f2] ""
       writeFile (dir </> compFileName pref f1 f2) stdout

main :: IO ()
main = do
  getArgs >>= \case
                (d:_) -> piecewiseAnalyse d
                _     -> putStrLn "usage: piecewise-analyse <dir>"
