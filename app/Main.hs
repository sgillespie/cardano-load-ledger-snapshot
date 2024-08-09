{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Cardano.Binary as Binary
import Cardano.Chain.Epoch.File (mainnetEpochSlots)
import Cardano.Ledger.Crypto (StandardCrypto ())
import Control.Exception (IOException (), try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Options.Applicative (Parser (), (<**>))
import qualified Options.Applicative as Optparse
import Ouroboros.Consensus.Cardano.Block (CardanoBlock (), CodecConfig ())
import Ouroboros.Consensus.Cardano.Node (protocolClientInfoCardano)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..), decodeExtLedgerState)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolClientInfo (..))
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..))

main :: IO ()
main = do
  opts <- parseOpts
  putStrLn $ "Loading " <> show (path opts) <> "."

  ledgerState' <- loadLedgerState (path opts)
  case ledgerState' of
    Nothing -> putStrLn "Error: Could not load snapshot!"
    Just ledger -> do
      putStrLn $ "Loaded " <> show ledgerState' <> "."
      reportLedgerState ledger

type CardanoExtLedgerState c = ExtLedgerState (CardanoBlock c)

-- * Parse CLI Options
parseOpts :: IO Options
parseOpts = Optparse.execParser (opts info)
  where
    opts = Optparse.info (optParser <**> Optparse.helper)
    info =
      Optparse.fullDesc
        <> Optparse.progDesc ""
        <> Optparse.header ""

newtype Options = Options
  {path :: FilePath}
  deriving (Eq, Show)

optParser :: Parser Options
optParser =
  Options <$> argPath
  where
    argPath =
      Optparse.strArgument $
        Optparse.metavar "FILE"

-- * Load ledger file
loadLedgerState :: FilePath -> IO (Maybe (CardanoExtLedgerState StandardCrypto))
loadLedgerState snapshot = do
  bytes <- readStateFile snapshot
  pure $ decodeSnapshot =<< bytes

readStateFile :: FilePath -> IO (Maybe ByteString.ByteString)
readStateFile snapshot = do
  try @IOException (ByteString.readFile snapshot) >>= \case
    Left _ -> pure Nothing
    Right res -> pure (Just res)

decodeSnapshot :: ByteString -> Maybe (CardanoExtLedgerState StandardCrypto)
decodeSnapshot bytes =
  case ledgerState' of
    Left _ -> Nothing
    Right res -> Just res
  where
    ledgerState' = Binary.decodeFullDecoder' "Ledger state file" decodeState bytes

    decodeState :: forall s. Binary.Decoder s (CardanoExtLedgerState StandardCrypto)
    decodeState =
      decodeExtLedgerState
        (decodeDisk codecConfig)
        (decodeDisk codecConfig)
        (decodeDisk codecConfig)

codecConfig :: CodecConfig (CardanoBlock StandardCrypto)
codecConfig = pClientInfoCodecConfig protoInfo
  where
    protoInfo = protocolClientInfoCardano mainnetEpochSlots

-- * Try to extract some meaningful information from ledger state
reportLedgerState :: CardanoExtLedgerState StandardCrypto -> IO ()
reportLedgerState state = pure ()
