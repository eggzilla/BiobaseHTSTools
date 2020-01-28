{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Console.CmdArgs
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Biobase.HTS.Library
import qualified Data.ByteString.Char8 as B
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
  if null samFilePath
    then print("No input" :: String)
    else do
      sams <- readSAMSs samFilePath
      mapM_ outputMappedEntries sams
      --let sam = head sams
      --let mapped_sam = filter (\entry -> (4 :: Int) /= mapq entry) (samEntries sam)
      --mapM_ (putStr . show) mapped_sam
      --let bitflag = myBit
      --let justBitSize = bitSizeMaybe bitflag
      --print justBitSize 
      --let ltestBit = testBit bitflag 4
      --print ltestBit

--outputMappedEntries :: V.Vector -> [SAMEntries] -> IO ()
--outputMappedEntries genomeVector sam
  -- | null sam = 
  -- | otherwise =
  --let mapped_sam = filter (\entry -> (4 :: Int) /= mapq entry) (samEntries sam)
  --let mapped_sam = samEntries sam
  --mapM_ (B.putStrLn pos) mapped_sam


outputMappedEntries :: SAM -> IO ()
outputMappedEntries sam = do
  --let mapped_sam = filter (\entry -> (4 :: Int) /= mapq entry) (samEntries sam)
  let mapped_sam = samEntries sam
  mapM_ (print . pos) mapped_sam

--bitCheck :: Bit Int -> Bool
myBit :: Int
myBit = bit 1
