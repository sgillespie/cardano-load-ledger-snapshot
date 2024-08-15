{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

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
import Cardano.Ledger.Alonzo.Data (Datum (..))
import Cardano.Ledger.Alonzo.TxOut (AlonzoTxOut (..))
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.Core (EraTxOut (..))
import Cardano.Ledger.Crypto (StandardCrypto ())
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Shelley (ShelleyTxOut)
import Cardano.Ledger.Shelley.Governance (EraGov (..))
import qualified Cardano.Ledger.Shelley.LedgerState as LedgerState
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Control.Exception (IOException (), try)
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.SOP.Strict (NP (..), fn, hap, type (-.->))
import Data.Text (Text ())
import qualified Data.Text as Text
import Data.Word (Word64 (), Word8 ())
import Formatting.Buildable (Buildable (..))
import Formatting.FromBuilder (FromBuilder (..))
import qualified Ouroboros.Consensus.Cardano.Block as Block
import Ouroboros.Consensus.Cardano.Node (protocolClientInfoCardano)
import Ouroboros.Consensus.HardFork.Combinator (LedgerState (..))
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState ())
import qualified Ouroboros.Consensus.Ledger.Extended as ExtLedger
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolClientInfo (..))
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock (..))
import Ouroboros.Consensus.Shelley.Ledger.Ledger (LedgerState (..))
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..), EncodeDisk (..))

type CardanoExtLedgerState c =
  ExtLedgerState (Block.CardanoBlock c)

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

codecConfig :: Block.CodecConfig (Block.CardanoBlock StandardCrypto)
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
trimLedgerState (CardanoLedgerState extLedger blockNo) =
  CardanoLedgerState (trimExtLedgerState extLedger) blockNo

trimExtLedgerState
  :: CardanoExtLedgerState StandardCrypto
  -> CardanoExtLedgerState StandardCrypto
trimExtLedgerState ledger =
  case ExtLedger.ledgerState ledger of
    HardForkLedgerState hfState ->
      let newHfState = hap (fn id :* f) hfState
       in updateLedgerState $ HardForkLedgerState newHfState
  where
    updateLedgerState
      :: Block.LedgerState
          (Block.CardanoBlock StandardCrypto)
      -> ExtLedgerState (Block.CardanoBlock StandardCrypto)
    updateLedgerState ledgerState' = ledger{ExtLedger.ledgerState = ledgerState'}

    f
      :: NP
          (Block.LedgerState -.-> Block.LedgerState)
          (Block.CardanoShelleyEras StandardCrypto)
    f =
      fn (trimState trimShelleyTxOut)
        :* fn (trimState trimAllegraTxOut)
        :* fn (trimState trimMaryTxOut)
        :* fn (trimState trimAlonzoTxOut)
        :* fn (trimState trimBabbageTxOut)
        :* fn (trimState trimConwayTxOut)
        :* Nil

trimState
  :: (EraGov era)
  => (TxOut era -> TxOut era)
  -> LedgerState (ShelleyBlock proto era)
  -> LedgerState (ShelleyBlock proto era)
trimState f ledger = ledger{shelleyLedgerState = newEpochState'}
  where
    newEpochState = shelleyLedgerState ledger
    newEpochState' = newEpochState{LedgerState.nesEs = epochState'}

    epochState = LedgerState.nesEs newEpochState
    epochState' = epochState{LedgerState.esLState = ledgerState'}

    ledgerState = LedgerState.esLState epochState
    ledgerState' = ledgerState{LedgerState.lsUTxOState = utxoState'}

    utxoState = LedgerState.lsUTxOState ledgerState
    utxoState' = utxoState{LedgerState.utxosUtxo = utxos'}

    utxos = LedgerState.utxosUtxo utxoState
    utxos' = UTxO utxo'

    utxo = unUTxO utxos
    utxo' = Map.map f utxo

trimShelleyTxOut
  :: ShelleyTxOut (Block.ShelleyEra StandardCrypto)
  -> ShelleyTxOut (Block.ShelleyEra StandardCrypto)
trimShelleyTxOut = id

trimAllegraTxOut
  :: ShelleyTxOut (Block.AllegraEra StandardCrypto)
  -> ShelleyTxOut (Block.AllegraEra StandardCrypto)
trimAllegraTxOut = id

trimMaryTxOut
  :: ShelleyTxOut (Block.MaryEra StandardCrypto)
  -> ShelleyTxOut (Block.MaryEra StandardCrypto)
trimMaryTxOut (ShelleyTxOut addr val) = ShelleyTxOut addr val'
  where
    (MaryValue coin _) = val
    val' = MaryValue coin mempty

trimAlonzoTxOut
  :: AlonzoTxOut (Block.AlonzoEra StandardCrypto)
  -> AlonzoTxOut (Block.AlonzoEra StandardCrypto)
trimAlonzoTxOut (AlonzoTxOut addr val _) = AlonzoTxOut addr val' hashes'
  where
    (MaryValue coin _) = val
    val' = MaryValue coin mempty

    hashes' = SNothing

trimBabbageTxOut
  :: BabbageTxOut (Block.BabbageEra StandardCrypto)
  -> BabbageTxOut (Block.BabbageEra StandardCrypto)
trimBabbageTxOut (BabbageTxOut addr val _ _) = BabbageTxOut addr val' datum' ref'
  where
    (MaryValue coin _) = val

    val' = MaryValue coin mempty
    datum' = NoDatum
    ref' = SNothing

trimConwayTxOut
  :: BabbageTxOut (Block.ConwayEra StandardCrypto)
  -> BabbageTxOut (Block.ConwayEra StandardCrypto)
trimConwayTxOut (BabbageTxOut addr val _ _) = BabbageTxOut addr val' datum' ref'
  where
    (MaryValue coin _) = val

    val' = MaryValue coin mempty
    datum' = NoDatum
    ref' = SNothing
