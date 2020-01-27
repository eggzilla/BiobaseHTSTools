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
import Data.Bits
import Data.Maybe

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
  if False-- null samFilePath
    then print("No input" :: String)
    else do
      sams <- readSAMSs samFilePath 
      let sam = head sams
      let mapped_sam = V.filter (\entry -> (4 :: Int) /= mapq entry) (samEntries sam)
      mapM_ (putStr . show) mapped_sam
      --let bitflag = myBit
      --let justBitSize = bitSizeMaybe bitflag
      --print justBitSize 
      --let ltestBit = testBit bitflag 4
      --print ltestBit

--bitCheck :: Bit Int -> Bool
myBit :: Int
myBit = bit 1
