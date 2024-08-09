{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Cardano.Binary as Binary
import Cardano.Chain.Epoch.File (mainnetEpochSlots)
import Cardano.Ledger.Crypto (StandardCrypto ())
import Control.Exception (IOException (), try)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text ())
import qualified Data.Text as Text
import Formatting.Buildable (Buildable (..))
import Formatting.FromBuilder (FromBuilder (..))
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

  let snapshotFile = path opts

  putStrLn $ "Loading " <> snapshotFile <> "."

  ledgerState' <- loadLedgerState snapshotFile
  case ledgerState' of
    Left e -> putStrLn $ "Error: " <> show e
    Right ledger -> do
      putStrLn $ "Loaded " <> snapshotFile <> "."
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
loadLedgerState :: FilePath -> IO (Either Text (CardanoExtLedgerState StandardCrypto))
loadLedgerState snapshot = do
  bytes <- readStateFile snapshot
  pure (decodeSnapshot =<< bytes)

readStateFile :: FilePath -> IO (Either Text ByteString)
readStateFile snapshot = first toText' <$> tryRead
  where
    tryRead = try @IOException (ByteString.readFile snapshot)
    toText' = Text.pack . show

decodeSnapshot :: ByteString -> Either Text (CardanoExtLedgerState StandardCrypto)
decodeSnapshot bytes = first toText ledgerState'
  where
    ledgerState' = Binary.decodeFullDecoder' "Ledger state file" decodeLedgerState bytes

decodeLedgerState :: Binary.Decoder s (CardanoExtLedgerState StandardCrypto)
decodeLedgerState =
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

toText :: (Buildable b) => b -> Text
toText = fromBuilder . build
