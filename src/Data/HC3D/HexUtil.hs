{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.HC3D.HexUtil where

import Data.ByteString (ByteString)
import Data.Char
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Base16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word
import Formatting

data HexLine = HexLine
    { offset :: Int
    , bytes  :: [Word8]
    , ascii  :: Text
    }

tupleToHexLine :: (Int, [Word8], Text) -> HexLine
tupleToHexLine (offset, bytes, ascii) = HexLine{..}

data HexBlock = HexBlock [HexLine]

bsToHexBlock :: Int
             -> Int
             -> ByteString
             -> HexBlock
bsToHexBlock initOffset lineWidth bs =
  let
    bss     = chunks lineWidth bs
    offsets = scanl (\n b -> n + B.length b) initOffset bss
    bytess  = map B.unpack bss
    asciis  = map (T.pack . replaceNonPrintable . C.unpack) bss
  in
    HexBlock $ map tupleToHexLine $ zip3 offsets bytess asciis

-- Replaces non-printable characters in a String with '.'
replaceNonPrintable :: String -> String
replaceNonPrintable ss = map (\x -> if isPrint x then x else '.') ss

-- Split into chunks of length n
chunks :: Int -> ByteString -> [ByteString]
chunks n bs
    | C.null bs = []
    | otherwise = C.take n bs : chunks n (C.drop n bs)

haskellPrintHexBlock :: Int -> Int -> HexBlock -> Text
haskellPrintHexBlock initIndent indent (HexBlock ls) =
  let
    ot  = TL.pack $ replicate initIndent ' '
    f l = format (text % text) 
              ot 
              (TL.fromStrict $ haskellPrintHexLine indent l)
    ss  = TL.intercalate "\n" $ map f ls
  in
    TL.toStrict $
        format ( text % "HexBlock [\n"
               % text
               % text % "]\n"
               ) ot ss ot

haskellPrintHexLine :: Int -> HexLine -> Text
haskellPrintHexLine indent HexLine{..} =
  let
    dt = TL.pack $ replicate indent ' '
    os = format (left 6 '0') offset
    bs = TL.intercalate ", " $ map (format ("0x" % hex)) bytes
    ss = format (text) $ TL.fromStrict ascii
  in
    TL.toStrict $
        format ( text
               % "HexLine " 
               % text
               % "[ " % text % "] \""
               % text 
               % "\""
               ) dt os bs ss

prettyPrintHexBlock :: HexBlock -> Text
prettyPrintHexBlock (HexBlock ls) =
  let
    nColBytes = maximum $ map (T.length . ascii) ls
  in
    T.intercalate "\n" $ map (prettyPrintHexLine nColBytes) ls

prettyPrintHexLine :: Int -> HexLine -> Text
prettyPrintHexLine nColBytes HexLine{..} =
  let
    es = TL.concat $ replicate (nColBytes - T.length ascii) "   "
    os = format (left 6 '0' % ":") offset
    bs = TL.concat $ map (format (" " % hex)) bytes
    ss = format (": " % text) $ TL.fromStrict ascii
  in
    TL.toStrict $
      format (text % text % text % text) os bs es ss

