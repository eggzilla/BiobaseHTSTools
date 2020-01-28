{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- This module provides a parser and data structure for SAM format
module Biobase.HTS.SAMParser where

import Prelude hiding (takeWhile, take)
import Data.Attoparsec.ByteString.Char8 hiding (isSpace)
import Data.Attoparsec.Combinator
import qualified Data.Attoparsec.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Lazy.Char8 as C
import System.Directory
import Data.Char
--import qualified Data.Vector as V
import Data.Bits.Bitwise
import qualified Data.Either.Unwrap as E

-- | SAM
-- For specification see https://samtools.github.io/hts-specs/SAMv1.pdf
data SAM = SAM
  { samHeader :: [SAMHeaderEntry],
    samEntries :: [SAMEntry]
  } deriving (Eq)

data SAMHeaderEntry = SAMHeaderEntry
  { headerid :: B.ByteString,
    headervalue :: B.ByteString
  } deriving (Eq)
         
data SAMEntry = SAMEntry
  { qname :: B.ByteString,
    flag :: [Bool],
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
  } deriving (Eq)

data SAMFlag = ReadMapped | ReadMappedInProperPair | ReadUnmapped | ReadReverseStrand | MateReverseStrand | FirstInPair | SecondInPair | NotPrimaryAlignment | ReadFailsQualityCheck | ReadIsDuplicate | SupplementaryAlignment
     deriving (Eq)
              
instance Show SAM where
  show (SAM _samHeader _samEntries) = concatMap show _samHeader ++ concatMap show  _samEntries

instance Show SAMHeaderEntry where
  show (SAMHeaderEntry _headerid _headervalue) =
    (B.unpack _headerid) ++ "\t" ++ (B.unpack _headervalue) ++ "\n"

instance Show SAMEntry where
  show (SAMEntry  _qname _flag _rname _pos _mapq _cigar _rnext _pnext _tlen _seq _qual _rest) =
    (B.unpack _qname) ++ "\t" ++ (show _flag) ++ "\t" ++ (show _pos) ++ "\t" ++ show _mapq ++ "\t" ++ B.unpack _cigar ++ "\t" ++ (B.unpack _rnext) ++ "\t" ++ show _pnext ++  "\t" ++ show _tlen ++ "\t" ++ B.unpack _seq ++ "\t" ++ B.unpack _qual  ++ B.unpack _rest ++ "\n"



-- | reads and parses SAMs from provided filePath
readSAMSs :: String -> IO [SAM]
readSAMSs filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
     then parseSAMs <$> C.readFile filePath
     else fail "# SAM file \"%s\" does not exist\n" filePath

parseSAMs :: C.ByteString -> [SAM]
parseSAMs = go
  where go xs = case L.parse genParseSAM xs of
          L.Fail remainingInput ctxts err  -> error $ "parseSAM failed! " ++ err ++ " ctxt: " ++ show ctxts ++ " head of remaining input: " ++ (C.unpack $ C.take 1000 remainingInput)
          L.Done remainingInput btr
            | C.null remainingInput  -> [btr]
            | otherwise              -> btr : go remainingInput

-- | reads and parses SAM from provided filePath
--readSAM :: String -> IO SAM
--readSAM filePath = do
--  fileExists <- doesFileExist filePath
--  if fileExists
--     then parseSAM <$> C.readFile filePath
--     else fail "# SAM file \"%s\" does not exist\n" filePath

--parseSAM :: C.ByteString -> SAM
--parseSAM inputBS = E.fromRight (L.parseOnly genParseSAM inputBS)
--  where go xs = case L.parse genParseSAM xs of
--          L.Fail remainingInput ctxts err  -> error $ "parseSAM failed! " ++ err ++ " ctxt: " ++ show ctxts ++ " head of remaining input: " ++ (C.unpack $ C.take 1000 remainingInput)
--          L.Done remainingInput btr
--            | C.null remainingInput  -> btr
--            | otherwise              -> btr : go remainingInput

genParseSAM :: Parser SAM
genParseSAM = do
  _samHeader <- many' (try genParseSAMHeaderEntry) <?> "SAM header entry"
  _ <- many1 (notChar '\n')
  _ <- endOfLine
  _samEntries <- many' (try genParseSAMEntry)  <?> "SAM entry"
  return $ SAM _samHeader _samEntries

genParseSAMHeaderEntry :: Parser SAMHeaderEntry
genParseSAMHeaderEntry = do
  _ <- char '@'
  _headerid <- take 2
  _headerentry <- takeWhile1 ((/=10) . ord) <?> "rest" -- 10 == '\n'
  _ <- char '\n'
  return $ SAMHeaderEntry _headerid _headerentry
         

genParseSAMEntry :: Parser SAMEntry
genParseSAMEntry = do
  _ <- lookAhead (notChar '@')
  _qname <- takeWhile1 ((/=9) . ord) <?> "qname"
  _ <- char '\t'
  _flag <- decimal <?> "flag"
  _ <- char '\t'
  _rname <- takeWhile1 ((/=9) . ord) <?> "rname"
  _ <- char '\t'
  _pos <- decimal <?> "pos"
  _ <- char '\t'
  _mapq <- decimal <?> "mapq"
  _ <- char '\t'
  _cigar <- takeWhile1 ((/=9) . ord) <?> "cigar"
  _ <- char '\t'
  _rnext <- takeWhile1 ((/=9) . ord) <?> "rnext"
  _ <- char '\t'
  _pnext <- decimal <?> "pnext"
  _ <- char '\t'
  _tlen <- decimal <?> "tlen"
  _ <- char '\t'
  _seq <- takeWhile1 ((/=9) . ord) <?> "seq"
  _ <- char '\t'
  _qual <- takeWhile1 ((/=9) . ord) <?> "qual"
  _ <- char '\t'
  _rest <- takeWhile1 ((/=10) . ord) <?> "rest" -- 10 == '\n'
  _ <- char '\n'
  return $ SAMEntry _qname (toListLE (toInteger _flag)) _rname _pos _mapq _cigar _rnext _pnext _tlen _seq _qual _rest

toLB :: B.ByteString -> C.ByteString
toLB = S.toLazyByteString . S.byteString
