module Data.HC3D.Binary where

import qualified Data.Serialize.Get     as G (Get, getWord16be, getWord16le,
                                              getWord8, runGet)
import qualified Data.Serialize.IEEE754 as F (getFloat32be, getFloat32le,
                                              putFloat32be, putFloat32le)
import qualified Data.Serialize.Put     as P (Putter, putWord16be, putWord16le,
                                              putWord8, runPut)
import           Data.Word              (Word16, Word8)


data Processor
    = Intel
    | DEC
    | MIPS
    deriving (Eq, Show)


data GetPro = GetPro
    { getProWord8  :: G.Get Word8
    , getProWord16 :: G.Get Word16
    , getProFloat  :: G.Get Float
    }


data PutPro = PutPro
    { putProWord8  :: P.Putter Word8
    , putProWord16 :: P.Putter Word16
    , putProFloat  :: P.Putter Float
    }


getPro :: Processor -> GetPro
getPro p = case p of
    Intel -> intelGetPro
    DEC   -> decGetPro
    MIPS  -> mipsGetPro


putPro :: Processor -> PutPro
putPro p = case p of
    Intel -> intelPutPro
    DEC   -> decPutPro
    MIPS  -> mipsPutPro


intelGetPro :: GetPro
intelGetPro = GetPro
    { getProWord8  = G.getWord8
    , getProWord16 = G.getWord16le
    , getProFloat  = F.getFloat32le
    }


decGetPro :: GetPro
decGetPro = GetPro
    { getProWord8  = G.getWord8
    , getProWord16 = G.getWord16le
    , getProFloat  = do
        a <- G.getWord8
        b <- G.getWord8
        c <- G.getWord8
        d <- G.getWord8
        pure $ word8sToFloat (c, d, a, b - if a == 0 then 0 else 1)
    }


mipsGetPro :: GetPro
mipsGetPro = GetPro
    { getProWord8  = G.getWord8
    , getProWord16 = G.getWord16be
    , getProFloat  = F.getFloat32be
    }


intelPutPro :: PutPro
intelPutPro = PutPro
    { putProWord8  = P.putWord8
    , putProWord16 = P.putWord16le
    , putProFloat  = F.putFloat32le
    }


decPutPro :: PutPro
decPutPro = PutPro
    { putProWord8  = P.putWord8
    , putProWord16 = P.putWord16le
    , putProFloat  = \f -> do
        let (a, b, c, d) = floatToWord8s f
        P.putWord8 c
        P.putWord8 (d + if d == 0 then 0 else 1)
        P.putWord8 a
        P.putWord8 b
    }


mipsPutPro :: PutPro
mipsPutPro = PutPro
    { putProWord8  = P.putWord8
    , putProWord16 = P.putWord16be
    , putProFloat  = F.putFloat32be
    }


floatToWord8s :: Float -> (Word8, Word8, Word8, Word8)
floatToWord8s f = case r of
    Right w -> w
    Left  _ -> error "Could not convert Float to Word8 tuple"
  where
    put = F.putFloat32le
    get = do
        a <- G.getWord8
        b <- G.getWord8
        c <- G.getWord8
        d <- G.getWord8
        pure (a, b, c, d)
    r = G.runGet get $ P.runPut $ put f


word8sToFloat :: (Word8, Word8, Word8, Word8) -> Float
word8sToFloat (a, b, c, d) = case r of
    Right f -> f
    Left  _ -> error "Could not convert Word8 tuple to Float"
  where
    put = do
        P.putWord8 a
        P.putWord8 b
        P.putWord8 c
        P.putWord8 d
    get = F.getFloat32le
    r = G.runGet get $ P.runPut $ put

