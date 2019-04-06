{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- This attoparsec module
module Biobase.HTS.SAMParser where

import Prelude hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8 hiding (isSpace)
import qualified Data.Attoparsec.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BB
import qualified Data.Vector as V
import System.Directory
import Data.Char
import Control.Monad
import Debug.Trace
import Text.Printf

-- | SAM
-- For specification see https://samtools.github.io/hts-specs/SAMv1.pdf
data SAM = SAM
  { samHeader :: SAMHeader,
    samEntries :: [SAMEntry],
  } deriving (Show)

data SAMHeader = SAMHeader
  { headerEntries :: [B.ByteString]
  } deriving (Show)

data SAMEntry = SAMEntry
  { qname :: B.ByteString,
    flag :: Int,
    rname :: B.ByteString,
    pos :: Int,
    mapq :: Int,
    cigar :: B.ByteString,
    rnext :: B.ByteString,
    pnext :: Int,
    tlen :: Int,
    seq :: B.ByteString,
    qual :: B.ByteString,
    rest :: B.ByteString
  } deriving (Show)


-- | reads and parses SAMs from provided filePath
readSAMSs :: String -> IO [SAM]
readSAMSs filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
     then parseSAMSs <$> B.readFile filePath
     else fail "# SAM file \"%s\" does not exist\n" filePath

parseSAMs :: B.ByteString -> [SAM]
parseSAMs = go
  where go xs = case L.parse genParseSAM xs of
          L.Fail remainingInput ctxts err  -> error $ "parseSAM failed! " ++ err ++ " ctxt: " ++ show ctxts ++ " head of remaining input: " ++ (B.unpack $ B.take 1000 remainingInput)
          L.Done remainingInput btr
            | B.null remainingInput  -> [btr]
            | otherwise              -> btr : go remainingInput

genParseSAM :: Parser SAM
genParseSAM = do
  _samHeader <- genParseSAMHeader <?> "Header"
  many1 (notChar '\n')
  endOfLine
  _samEntries <- many (try genParseSAMEntry)  <?> "SAM entry"
  return $ SAM _samHeader _samEntries


genParseSAMEntry :: Parser SAMEntry
genParseSAMEntry = do
  _qname <- takeWhile1 ((/=9) . ord) <?> "qname"
  char '\t'
  _flag <- decimal <?> "flag"
  char '\t'
  _rname <- takeWhile1 ((/=9) . ord) <?> "rname"
  char '\t'
  _pos <- decimal <?> "pos"
  char '\t'
  _mapq <- decimal <?> "mapq"
  char '\t'
  _cigar <- takeWhile1 ((/=9) . ord) <?> "cigar"
  char '\t'
  _rnext <- takeWhile1 ((/=9) . ord) <?> "rnext"
  char '\t'
  _tlen <- decimal <?> "tlen"
  char '\t'
  _seq <- takeWhile1 ((/=9) . ord) <?> "seq"
  char '\t'
  _qual <- takeWhile1 ((/=9) . ord) <?> "qual"
  char '\t'
  _rest <- takeWhile1 ((/=10) . ord) <?> "rest" -- 10 == '\n'
  char '\n'
  return $ SAMEntry _qname _flag _rname _pos _mapq _cigar _rnext _tlen _seq _qual _rest

--IUPAC amino acid with gap
--aminoacidLetters :: Char -> Bool
aminoacidLetters = inClass "ARNDCQEGHILMFPSTWYVBZX-"

--IUPAC nucleic acid characters with gap
--nucleotideLetters :: Char -> Bool
nucleotideLetters = inClass "AGTCURYSWKMBDHVN-."

--IUPAC nucleic acid characters with gap
--bioLetters :: Char -> Bool
bioLetters = inClass "ABCDEFGHIJKLMNOPQRSTUVWXYZ.-"


toLB :: C.ByteString -> B.ByteString
toLB = S.toLazyByteString . S.byteString
