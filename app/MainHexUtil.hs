{-# LANGUAGE RecordWildCards #-}
{-|
Description : Command-line utility for dumping hex (for testing)
Copyright   : (c) Jonathan Merritt, 2016
Maintainer  : j.s.merritt@gmail.com

Extracts hex from files in a format suitable for inserting direcly into
Haskell source files for testing purposes.
-}
module Main where

import           Data.HC3D.HexUtil   (HexBlock, encode, haskellText,
                                      hexdumpText)

import qualified Data.ByteString     as B
import qualified Data.Text.IO        as TIO
import           Options.Applicative


main :: IO ()
main = getCmdOptions >>= doExport


doExport :: CmdOptions -> IO ()
doExport CmdOptions{..} = do
    inbs <- B.readFile inFileName
    let
        dn = maybe 0 id skipN
        bs' = B.drop dn inbs
        bs = case takeN of
            Just n  -> B.take n bs'
            Nothing -> bs'
        hb = encode dn 16 bs
        ss = if hexDump
                 then hexdumpText hb
                 else haskellText 4 4 hb
    TIO.putStr ss


getCmdOptions :: IO CmdOptions
getCmdOptions = execParser opts
  where
    opts = info (helper <*> cmdOptionsParser)
        (  fullDesc
        <> progDesc "Hex dumping utility"
        <> header "hexutil - dumps hex as inline Haskell."
        )


data CmdOptions = CmdOptions
    { inFileName :: String
    , skipN      :: Maybe Int
    , takeN      :: Maybe Int
    , hexDump    :: Bool
    }


cmdOptionsParser :: Parser CmdOptions
cmdOptionsParser = CmdOptions
    <$> argument str (metavar "FILE")
    <*> optional (option auto
        (  long "skip"
        <> short 's'
        <> metavar "offset"
        <> help "Skip this many bytes from the beginning of the input."
        ))
    <*> optional (option auto
        (  long "take"
        <> short 'n'
        <> metavar "length"
        <> help "Interpret only this many bytes of input."
        ))
    <*> switch
        (  long "hexdump"
        <> help "Export in hexdump canonical format rather than as Haskell."
        )

