{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-|
Description : Hex Utilities (for testing)
Copyright   : (c) Jonathan Merritt, 2016
Maintainer  : j.s.merritt@gmail.com

Utility functions for extracting hex that can be inserted directly into
Haskell source files for testing purposes.
-}
module Data.HC3D.HexUtil (
    -- * Types
    HexBlock(..)
  , HexLine(..)
    -- * Functions
  , decode
  , encode
  , haskellText
  , hexdumpText
) where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C
import           Data.Char              (isPrint)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           Data.Word              (Word8)
import           Formatting             (format, left, text, (%))


------------------------------------------------------------------------------
-- DATA TYPES
------------------------------------------------------------------------------


-- | Line of hex from a file.
--
--   'HexLine' redundantly represents a line of hex from some source file. It
--   contains redunant information so that it can be read more conveniently in
--   tests.
data HexLine = HexLine
    { offset :: Int         -- ^ offset of the first byte within the file
    , base16 :: ByteString  -- ^ data stored base-16 encoded
    , ascii  :: T.Text      -- ^ printable ASCII characters
    }


-- | Block of hex from a file.
data HexBlock = HexBlock [HexLine]


------------------------------------------------------------------------------
-- EXPORTED FUNCTIONS
------------------------------------------------------------------------------


-- | Decodes a 'HexBlock' into its raw 'ByteString'.
--
--   In theory, this can fail. For example, the 'HexBlock' can contain
--   non-hex characters. However, since 'decode' is used primarily for
--   testing, it should be valid simply to fail the test if the decode itself
--   fails.
decode :: HexBlock -> Maybe ByteString
decode (HexBlock ls) = fmap B.concat $ sequence $ map decodeLine ls


-- | Encodes a 'ByteString' into a 'HexBlock'.
encode :: Int         -- ^ initial offset of the block (documentation only!)
       -> Int         -- ^ width of a line of the block in bytes
       -> ByteString  -- ^ bytestring to encode
       -> HexBlock    -- ^ produced block
encode initOffset lineWidth bs =

  let
    bss     = chunks lineWidth bs
    offsets = scanl (\n b -> n + B.length b) initOffset bss
    base16s = map B16.encode bss
    asciis  = map (T.pack . replaceNonPrintable . C.unpack) bss

  in
    HexBlock $ map tupleToHexLine $ zip3 offsets base16s asciis

-- | Converts a 'HexBlock' to 'Text' suitable for inclusion in a Haskell file.
haskellText :: Int       -- ^ initial indent size
            -> Int       -- ^ indent width
            -> HexBlock  -- ^ block to convert
            -> T.Text    -- ^ text representing the block
haskellText initIndent indent (HexBlock ls) =
  let
    ot  = TL.pack $ replicate initIndent ' '
    f l = format (text % text)
              ot
              (TL.fromStrict $ haskellHexLine indent l)
    ss  = TL.intercalate "\n" $ map f ls

  in
    TL.toStrict $
        format ( text % "HexBlock [\n"
               % text % "\n"
               % text % "]\n"
               ) ot ss ot

-- | Converts a 'HexBlock' to 'Text' in a format similar to the hexdump tool.
hexdumpText :: HexBlock -> T.Text
hexdumpText (HexBlock ls) =
  let
    nColBytes = maximum $ map (T.length . ascii) ls
  in
    T.intercalate "\n" $ map (hexdumpHexLine nColBytes) ls


------------------------------------------------------------------------------
-- INTERNAL FUNCTIONS
------------------------------------------------------------------------------


-- | Decodes a line of hex.
decodeLine :: HexLine -> Maybe ByteString
decodeLine hl = if (B.null . snd) d then Just (fst d) else Nothing
  where
    d = B16.decode $ base16 hl


-- | Constructs a HexLine from a tuple.
tupleToHexLine :: (Int, ByteString, T.Text) -> HexLine
tupleToHexLine (offset, base16, ascii) = HexLine{..}


-- | Replaces non-printable characters in a String with '.'
replaceNonPrintable :: String -> String
replaceNonPrintable ss = map (\x -> if isPrint x then x else '.') ss


-- | Split into chunks of length n
chunks :: Int -> ByteString -> [ByteString]
chunks n bs
    | C.null bs = []
    | otherwise = C.take n bs : chunks n (C.drop n bs)


-- | Prints a 'HexLine' in a format suitable for a Haskell file.
haskellHexLine :: Int -> HexLine -> T.Text
haskellHexLine indent HexLine{..} =
  let
    dt = TL.pack $ replicate indent ' '
    os = format (left 6 '0') offset
    bs = TL.pack $ C.unpack base16
    ss = format (text) $ TL.fromStrict ascii
  in
    TL.toStrict $
        format ( text
               % "HexLine "
               % text
               % " \""
               % text
               % "\" \""
               % text
               % "\""
               ) dt os bs ss


-- | Prints a 'HexLine' in a format suitable for hexdump.
hexdumpHexLine :: Int -> HexLine -> T.Text
hexdumpHexLine nColBytes HexLine{..} =
  let
    es = TL.concat $ replicate (nColBytes - T.length ascii) "   "
    os = format (left 6 '0' % ":") offset
    bs = TL.intercalate " " $ map (TL.pack . C.unpack) $ chunks 2 base16
    ss = format (": " % text) $ TL.fromStrict ascii
  in
    TL.toStrict $
      format (text % text % text % text) os bs es ss

