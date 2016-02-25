{-# LANGUAGE OverloadedStrings #-}

module Data.HC3D.ParseSpec where

import Data.HC3D.HexUtil (HexBlock(..), HexLine(..))
import qualified Data.HC3D.HexUtil as HU (decode)
import Data.HC3D.Parse ()
import Data.HC3D.Types (Hdr(..))

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Serialize as DS
import qualified Data.Serialize.IEEE754 as DS
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)


suite :: TestTree
suite = testGroup "Parse Test Suite"
    [ testAdHocHeaderGet
    ]


testAdHocHeaderGet =
  let
    e :: (Eq a, Show a) => String -> a -> a -> IO ()
    e = assertEqual

    nullTime :: ByteString
    nullTime = DS.runPut $ do
        DS.putWord32le 0x00000000

    times :: ByteString
    times = DS.runPut $ do
        DS.putFloat32le 2.72
        DS.putFloat32le 5.4
        DS.putFloat32le 7.32
        DS.putByteString (B.concat $ replicate 15 nullTime)

    noDisplay :: ByteString
    noDisplay = DS.runPut $ do
        DS.putWord8 0x00

    display :: ByteString
    display = DS.runPut $ do
        DS.putWord8 0x01
        DS.putWord8 0x01
        DS.putWord8 0x01
        DS.putByteString (B.concat $ replicate 15 noDisplay)

    nullLabel :: ByteString
    nullLabel = B.pack $ replicate 4 0x00

    labels :: ByteString
    labels = B.append "RIC RHS RTO " (B.concat $ replicate 15 nullLabel)

  in
    testCase "decode header (ad hoc)" $ do
        bs  <- hexBlockIO eb015prHeader
        hdr <- (either fail pure $ DS.decode bs) :: (IO Hdr)
        _   <- e "paramOfs"        0x02           $ paramOfs hdr
        _   <- e "magic"           0x50           $ magic hdr
        _   <- e "nPoints"         26             $ nPoints hdr
        _   <- e "nAnalog"         64             $ nAnalog hdr
        _   <- e "firstFrame"      1              $ firstFrame hdr
        _   <- e "lastFrame"       450            $ lastFrame hdr
        _   <- e "interpGap"       10             $ interpGap hdr
        _   <- e "scale3D"         (-0.083333336) $ scale3D hdr
        _   <- e "dataStart"       11             $ dataStart hdr
        _   <- e "nAnalogPerFrame" 4              $ nAnalogPerFrame hdr
        _   <- e "frameRate3D"     50.0           $ frameRate3D hdr
        _   <- e "labelAndRange"   0              $ labelAndRange hdr
        _   <- e "labelRangeBlock" 0              $ labelRangeBlock hdr
        _   <- e "charLabels4"     12345          $ charLabels4 hdr
        _   <- e "nEvents"         3              $ nEvents hdr
        _   <- e "eventTimes"      times          $ eventTimes hdr
        _   <- e "eventDisplay"    display        $ eventDisplay hdr
        _   <- e "eventLabels"     labels         $ eventLabels hdr
        return ()


------------------------------------------------------------------------------
-- HEX BLOCKS
------------------------------------------------------------------------------


-- | HexBlockIO is suitable for HUnit tests which happen in IO.
hexBlockIO :: HexBlock -> IO ByteString
hexBlockIO = maybe (fail "could not decode HexBlock") pure . HU.decode


-- | Header block (512 bytes) from Eb015pr.c3d.
eb015prHeader :: HexBlock
eb015prHeader = 
    HexBlock 
    [ HexLine 000000 "02501a0040000100c2010a00abaaaabd" ".P..@...Â...«ªª½"
    , HexLine 000016 "0b000400000048420000000000000000" "......HB........"
    , HexLine 000032 "00000000000000000000000000000000" "................"
    , HexLine 000048 "00000000000000000000000000000000" "................"
    , HexLine 000064 "00000000000000000000000000000000" "................"
    , HexLine 000080 "00000000000000000000000000000000" "................"
    , HexLine 000096 "00000000000000000000000000000000" "................"
    , HexLine 000112 "00000000000000000000000000000000" "................"
    , HexLine 000128 "00000000000000000000000000000000" "................"
    , HexLine 000144 "00000000000000000000000000000000" "................"
    , HexLine 000160 "00000000000000000000000000000000" "................"
    , HexLine 000176 "00000000000000000000000000000000" "................"
    , HexLine 000192 "00000000000000000000000000000000" "................"
    , HexLine 000208 "00000000000000000000000000000000" "................"
    , HexLine 000224 "00000000000000000000000000000000" "................"
    , HexLine 000240 "00000000000000000000000000000000" "................"
    , HexLine 000256 "00000000000000000000000000000000" "................"
    , HexLine 000272 "00000000000000000000000000000000" "................"
    , HexLine 000288 "00000000000000000000393003000100" "..........90...."
    , HexLine 000304 "7b142e40cdccac40713dea4000000000" "{..@ÍÌ¬@q=ê@...."
    , HexLine 000320 "00000000000000000000000000000000" "................"
    , HexLine 000336 "00000000000000000000000000000000" "................"
    , HexLine 000352 "00000000000000000000000000000000" "................"
    , HexLine 000368 "00000000000000000101010000000000" "................"
    , HexLine 000384 "00000000000000000000000052494320" "............RIC "
    , HexLine 000400 "5248532052544f200000000000000000" "RHS RTO ........"
    , HexLine 000416 "00000000000000000000000000000000" "................"
    , HexLine 000432 "00000000000000000000000000000000" "................"
    , HexLine 000448 "00000000000000000000000000000000" "................"
    , HexLine 000464 "00000000000000000000000000000000" "................"
    , HexLine 000480 "00000000000000000000000000000000" "................"
    , HexLine 000496 "00000000000000000000000000000000" "................"
    ]
