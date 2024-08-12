{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Binary (Decoder (), FromCBOR (..))
import qualified Cardano.Binary as Binary
import Cardano.Chain.Epoch.File (mainnetEpochSlots)
import Cardano.Ledger.Crypto (StandardCrypto ())
import Control.Concurrent (threadDelay)
import Control.Exception (IOException (), try)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text ())
import qualified Data.Text as Text
import Data.Word (Word64 (), Word8 ())
import Formatting.Buildable (Buildable (..))
import Formatting.FromBuilder (FromBuilder (..))
import Options.Applicative (Parser (), (<**>))
import qualified Options.Applicative as Optparse
import Ouroboros.Consensus.Block.Abstract (BlockNo ())
import qualified Ouroboros.Consensus.Block.Abstract as Block
import Ouroboros.Consensus.Cardano.Block (CardanoBlock (), CodecConfig ())
import Ouroboros.Consensus.Cardano.Node (protocolClientInfoCardano)
import Ouroboros.Consensus.HeaderValidation (AnnTip (..), HeaderState (..))
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..), decodeExtLedgerState)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolClientInfo (..))
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..))

main :: IO ()
main = do
  opts <- parseOpts
  let snapshotFile = path opts

  putStrLn $ "Loading " <> snapshotFile <> "."

  -- Load the ledger state file into meory
  ledgerState' <- loadLedgerState snapshotFile

  -- Print some information about it
  case ledgerState' of
    Left e -> putStrLn $ "Error: " <> show e
    Right ledger -> do
      putStrLn $ "Loaded " <> snapshotFile <> "."
      reportLedgerState ledger

type CardanoExtLedgerState c = ExtLedgerState (CardanoBlock c)

data CardanoLedgerState c = CardanoLedgerState
  { clsState :: !(CardanoExtLedgerState c),
    clsEpochBlockNo :: !EpochBlockNo
  }

data EpochBlockNo
  = GenesisEpochBlockNo
  | EBBEpochBlockNo
  | EpochBlockNo !Word64
  deriving (Eq, Show)

instance FromCBOR EpochBlockNo where
  fromCBOR = do
    tag <- fromCBOR @Word8
    case tag of
      0 -> pure GenesisEpochBlockNo
      1 -> pure EBBEpochBlockNo
      2 -> EpochBlockNo <$> fromCBOR
      n -> fail $ "unexpected EpochBlockNo value " <> show n

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
loadLedgerState :: FilePath -> IO (Either Text (CardanoLedgerState StandardCrypto))
loadLedgerState snapshot = do
  bytes <- readStateFile snapshot
  pure (decodeSnapshot =<< bytes)

readStateFile :: FilePath -> IO (Either Text ByteString)
readStateFile snapshot = first toText' <$> tryRead
  where
    tryRead = try @IOException (ByteString.readFile snapshot)
    toText' = Text.pack . show

decodeSnapshot :: ByteString -> Either Text (CardanoLedgerState StandardCrypto)
decodeSnapshot bytes = first toText ledgerState'
  where
    ledgerState' = Binary.decodeFullDecoder' "Ledger state file" decodeLedgerState bytes

decodeLedgerState :: Decoder s (CardanoLedgerState StandardCrypto)
decodeLedgerState = CardanoLedgerState <$> extLedgerStateDecoder <*> Binary.fromCBOR
  where
    extLedgerStateDecoder =
      decodeExtLedgerState
        (decodeDisk codecConfig)
        (decodeDisk codecConfig)
        (decodeDisk codecConfig)

codecConfig :: CodecConfig (CardanoBlock StandardCrypto)
codecConfig = pClientInfoCodecConfig protoInfo
  where
    protoInfo = protocolClientInfoCardano mainnetEpochSlots

-- * Try to extract some meaningful information from ledger state
reportLedgerState :: CardanoLedgerState StandardCrypto -> IO ()
reportLedgerState (CardanoLedgerState state _) =
  putStrLn . info . getBlockNo $ state
  where
    info (Just blockNo) = "Tip is at block " <> show (Block.unBlockNo blockNo)
    info Nothing = "Tip is at origin"

getBlockNo :: CardanoExtLedgerState StandardCrypto -> Maybe BlockNo
getBlockNo (ExtLedgerState _ header) = annTipBlockNo <$> tip
  where
    HeaderState origin _ = header
    tip = Block.withOriginToMaybe origin

toText :: (Buildable b) => b -> Text
toText = fromBuilder . build
