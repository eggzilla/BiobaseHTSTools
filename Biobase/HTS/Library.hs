{-# LANGUAGE OverloadedStrings #-}
-- | Library for HTS fun

module Biobase.HTS.Library (

                    ) where

--import Data.Aeson
--import qualified Data.Text as T
--import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B

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

--wig
