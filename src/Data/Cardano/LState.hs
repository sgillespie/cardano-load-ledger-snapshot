{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.Cardano.LState
  ( CardanoExtLedgerState (),
    CardanoLedgerState (..),
    EpochBlockNo (..),
    loadLedgerState,
    saveLedgerState,
    trimLedgerState,
  ) where

import Cardano.Binary (Decoder (), Encoding (), FromCBOR (..), ToCBOR (..))
import qualified Cardano.Binary as Binary
import Cardano.Chain.Epoch.File (mainnetEpochSlots)
import Cardano.Ledger.Crypto (StandardCrypto ())
import Control.Exception (IOException (), try)
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Text (Text ())
import qualified Data.Text as Text
import Data.Word (Word64 (), Word8 ())
import Formatting.Buildable (Buildable (..))
import Formatting.FromBuilder (FromBuilder (..))
import Ouroboros.Consensus.Cardano.Block (CardanoBlock (), CodecConfig ())
import Ouroboros.Consensus.Cardano.Node (protocolClientInfoCardano)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState ())
import qualified Ouroboros.Consensus.Ledger.Extended as ExtLedger
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolClientInfo (..))
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..), EncodeDisk (..))

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

instance ToCBOR EpochBlockNo where
  toCBOR GenesisEpochBlockNo = toCBOR (0 :: Word8)
  toCBOR EBBEpochBlockNo = toCBOR (1 :: Word8)
  toCBOR (EpochBlockNo n) =
    toCBOR (2 :: Word8) <> toCBOR n

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
    ledgerState' = Binary.decodeFullDecoder "Ledger state file" decodeLedgerState bytes

decodeLedgerState :: Decoder s (CardanoLedgerState StandardCrypto)
decodeLedgerState = CardanoLedgerState <$> extLedgerStateDecoder <*> Binary.fromCBOR
  where
    extLedgerStateDecoder =
      ExtLedger.decodeExtLedgerState
        (decodeDisk codecConfig)
        (decodeDisk codecConfig)
        (decodeDisk codecConfig)

codecConfig :: CodecConfig (CardanoBlock StandardCrypto)
codecConfig = pClientInfoCodecConfig protoInfo
  where
    protoInfo = protocolClientInfoCardano mainnetEpochSlots

toText :: (Buildable b) => b -> Text
toText = fromBuilder . build

-- * Save ledger state
saveLedgerState :: CardanoLedgerState StandardCrypto -> FilePath -> IO (Either Text ())
saveLedgerState ledger snapshot = do
  let bs = Binary.serialize (encodeLedgerState ledger)

  res <- try @IOException (ByteString.writeFile snapshot bs)

  pure (first (Text.pack . show) res)

encodeLedgerState :: CardanoLedgerState StandardCrypto -> Encoding
encodeLedgerState ledger =
  encodeExtLedgerState' (clsState ledger)
    <> Binary.toCBOR (clsEpochBlockNo ledger)
  where
    encodeExtLedgerState' =
      ExtLedger.encodeExtLedgerState
        (encodeDisk codecConfig)
        (encodeDisk codecConfig)
        (encodeDisk codecConfig)

-- * Trim ledger state
trimLedgerState :: CardanoLedgerState StandardCrypto -> CardanoLedgerState StandardCrypto
trimLedgerState = id
