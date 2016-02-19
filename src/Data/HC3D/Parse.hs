{-# LANGUAGE RecordWildCards #-}

module Data.HC3D.Parse where

import Data.HC3D.Types

import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.IEEE754
import Data.Serialize.Put

instance Serialize Hdr where
    get = do
        paramOfs        <- getWord8
        magic           <- getWord8
        nPoints         <- getWord16le
        nAnalog         <- getWord16le
        firstFrame      <- getWord16le
        lastFrame       <- getWord16le
        interpGap       <- getWord16le
        scale3D         <- getFloat32le
        dataStart       <- getWord16le
        nAnalogPerFrame <- getWord16le
        frameRate3D     <- getFloat32le
        _res1           <- getBytes (147-13)
        labelAndRange   <- getWord16le
        labelRangeBlock <- getWord16le
        charLabels4     <- getWord16le
        nEvents         <- getWord16le
        _res2           <- getWord8
        eventTimes      <- getBytes (188-153)
        eventDisplay    <- getBytes (197-189)
        _res3           <- getWord8
        eventLabels     <- getBytes (234-199)
        _res4           <- getBytes (256-235)
        pure Hdr{..}

    put Hdr{..} = do
        putWord8      paramOfs
        putWord8      magic
        putWord16le   nPoints
        putWord16le   nAnalog
        putWord16le   firstFrame
        putWord16le   lastFrame
        putWord16le   interpGap
        putFloat32le  scale3D
        putWord16le   dataStart
        putWord16le   nAnalogPerFrame
        putFloat32le  frameRate3D
        putByteString _res1
        putWord16le   labelAndRange
        putWord16le   labelRangeBlock
        putWord16le   nEvents
        putWord8      _res2
        putByteString eventTimes
        putByteString eventDisplay
        putWord8     _res3
        putByteString eventLabels
        putByteString _res4

