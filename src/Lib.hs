module Lib
    (
        vorbisComments
      , getVorbisComments
      , blockData
    ) where

import Data.Serialize.Get
import Data.Word
import Data.Bits((.&.), shiftR)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Encoding as Enc
import Data.Encoding.ASCII
import Data.Encoding.UTF8

data VorbisCommentItem = VorbisCommentItem String String deriving (Eq, Ord, Show)

data VorbisComments = VorbisComments String [VorbisCommentItem] deriving (Show)

data BlockType = StreamInfo
               | Padding
               | Application
               | SeekTable
               | VorbisComment
               | CueSheet
               | Picture
               | Reserved
               | Invalid
               deriving (Eq, Show)

data MetadataBlockHeader = MetadataBlockHeader {
    isLast         :: Bool
  , blockType      :: BlockType
  , bytesToFollow :: Int
} deriving (Eq, Show)

data MetadataBlock = MetadataBlock {
    header    :: MetadataBlockHeader
  , blockData :: L.ByteString
} deriving (Eq, Show)

separator :: Char
separator = '='

getVorbisCommentItem :: Get VorbisCommentItem
getVorbisCommentItem  = do
  itemSize <- getWord32le
  itemData <- getByteString (fromEnum itemSize)
  let (keyData, valueData) = BSC.span (/= separator) itemData
      key   = Enc.decodeStrictByteString ASCII keyData
      value = Enc.decodeStrictByteString UTF8 (BS.tail valueData)
  return (VorbisCommentItem key value)

getVorbisCommentItems :: Int -> Get [VorbisCommentItem]
getVorbisCommentItems 0 = return []
getVorbisCommentItems n = do
  item <- getVorbisCommentItem
  rest <- getVorbisCommentItems(n - 1)
  return (item : rest)

getVorbisComments :: Get VorbisComments
getVorbisComments = do
  vendorSize <- getWord32le
  vendor     <- getByteString (fromIntegral vendorSize)
  itemCount  <- getWord32le
  items      <- getVorbisCommentItems(fromIntegral itemCount)
  return (VorbisComments (BSC.unpack vendor) items)

wBlockType :: Word8 -> BlockType
wBlockType w = case (w .&. 0x7F) of
  0   -> StreamInfo
  1   -> Padding
  2   -> Application
  3   -> SeekTable
  4   -> VorbisComment
  5   -> CueSheet
  6   -> Picture
  127 -> Invalid
  _   -> Reserved

getMetadataBlocks :: Bool -> Get [MetadataBlock]
getMetadataBlocks True = return []
getMetadataBlocks _ = do
  block <- getMetadataBlock
  rest <- getMetadataBlocks (isLast $ header block)
  return (block : rest)

getMetadataBlock :: Get MetadataBlock
getMetadataBlock = do
  h <- getMetadataBlockHeader
  d <- getLazyByteString (fromIntegral $ bytesToFollow h)
  return MetadataBlock { header = h, blockData = d }

getMetadataBlockHeader :: Get MetadataBlockHeader
getMetadataBlockHeader = do
  h <- getWord32be
  let i  = ((b0 .&. 0xFF) `shiftR`   7) == 1
      b0 = fromIntegral (h `shiftR` 24) :: Word8
      b1 = fromIntegral (h `shiftR` 16) :: Word8
      b2 = fromIntegral (h `shiftR`  8) :: Word8
      b3 = fromIntegral h               :: Word8
      l  = (fromIntegral(b3) + (256*fromIntegral(b2)) + (256*256*fromIntegral(b1)))
      bt = wBlockType b0
  return MetadataBlockHeader { isLast = i, blockType = bt, bytesToFollow = l }

vorbisComments :: Get [MetadataBlock]
vorbisComments = do
  _  <- getWord32be -- magic fLaC
  bs <- getMetadataBlocks False
  let fbs = filter (\x -> (blockType $ header x) == VorbisComment) bs
  return fbs
