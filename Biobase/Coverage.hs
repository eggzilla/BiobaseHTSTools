{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Console.CmdArgs
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Biobase.HTS.Library
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V

options :: Options
data Options = Options
  { samFilePath :: String
  } deriving (Show,Data,Typeable)

options = Options
  { samFilePath = def &= name "f" &= help "Path to SAM format file"
  } &= summary ("Coverage") &= help "Florian Eggenhofer - 2019" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  if null samFilePath
    then print("No input" :: String)
    else do
      sams <- readSAMSs samFilePath 
      let sam = head sams
      let mapped_sam = V.filter (\entry -> (4 :: Int) /= mapq entry) (samEntries sam)
      mapM_ (putStr . show) mapped_sam
      --print (B.unpack fqname)
