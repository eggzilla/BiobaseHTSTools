{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Console.CmdArgs
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

options :: Options
data Options = Options
  { geneIds :: String,
    geneIdFilePath :: String
  } deriving (Show,Data,Typeable)

options = Options
  { geneIds = def &= name "g" &= help "Ensembl gene ids (or NCBI locus tags) comma separated, e.g b0001,b0825",
    geneIdFilePath = def &= name "f" &= help "Path to file containing one Ensembl gene ids (or NCBI locus tags) per line, e.g b0001\nb0825\n"
  } &= summary ("GeneIdToGOTerms") &= help "Florian Eggenhofer - 2018" &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs options
  let geneIdsText = T.pack geneIds
  let geneIdList = T.splitOn "," geneIdsText
  if null geneIdFilePath
    then print("No input")
    else do
      geneIdFile <- TIO.readFile geneIdFilePath
      let fileGeneIds = T.lines geneIdFile
      print(head(fileGeneIds))
