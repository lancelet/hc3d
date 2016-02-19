module Data.HC3D.Types where

import Data.Word
import qualified Data.ByteString as B

data Hdr = Hdr 
    { paramOfs        :: {-# UNPACK #-} !Word8
    , magic           :: {-# UNPACK #-} !Word8
    , nPoints         :: {-# UNPACK #-} !Word16
    , nAnalog         :: {-# UNPACK #-} !Word16
    , firstFrame      :: {-# UNPACK #-} !Word16
    , lastFrame       :: {-# UNPACK #-} !Word16
    , interpGap       :: {-# UNPACK #-} !Word16
    , scale3D         :: {-# UNPACK #-} !Float
    , dataStart       :: {-# UNPACK #-} !Word16
    , nAnalogPerFrame :: {-# UNPACK #-} !Word16
    , frameRate3D     :: {-# UNPACK #-} !Float
    , _res1           :: {-# UNPACK #-} !B.ByteString
    , labelAndRange   :: {-# UNPACK #-} !Word16
    , labelRangeBlock :: {-# UNPACK #-} !Word16
    , charLabels4     :: {-# UNPACK #-} !Word16
    , nEvents         :: {-# UNPACK #-} !Word16
    , _res2           :: {-# UNPACK #-} !Word8
    , eventTimes      :: {-# UNPACK #-} !B.ByteString
    , eventDisplay    :: {-# UNPACK #-} !B.ByteString
    , _res3           :: {-# UNPACK #-} !Word8
    , eventLabels     :: {-# UNPACK #-} !B.ByteString
    , _res4           :: {-# UNPACK #-} !B.ByteString
    }

