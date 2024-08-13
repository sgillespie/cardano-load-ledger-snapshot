{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cardano.Ledger.Crypto (StandardCrypto ())
import Control.Monad (when)
import Data.Cardano.LState (CardanoExtLedgerState (), CardanoLedgerState (..))
import qualified Data.Cardano.LState as LState
import Options.Applicative (Parser (), (<**>))
import qualified Options.Applicative as Optparse
import Ouroboros.Consensus.Block.Abstract (BlockNo ())
import qualified Ouroboros.Consensus.Block.Abstract as Block
import Ouroboros.Consensus.HeaderValidation (AnnTip (..), HeaderState (..))
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()

main :: IO ()
main = do
  opts <- parseOpts
  let snapshotFile = path opts
      shouldTrim = trim opts

  putStrLn $ "Loading " <> snapshotFile <> "."

  -- Load the ledger state file into memory
  ledgerState' <- LState.loadLedgerState snapshotFile

  -- Print some information about it
  case ledgerState' of
    Left e -> putStrLn $ "Error: " <> show e
    Right ledger -> do
      putStrLn $ "Loaded " <> snapshotFile <> "."
      reportLedgerState ledger

      -- Trim ledger state in memory
      when shouldTrim $ do
        let newLedger = LState.trimLedgerState ledger
            snapshotFile' = snapshotFile <> ".trimmed"

        putStrLn $ "Writing " <> snapshotFile' <> " to disk."
        saved <- LState.saveLedgerState newLedger (snapshotFile <> ".trimmed")
        case saved of
          Left e -> putStrLn $ "Error: " <> show e
          Right () -> putStrLn $ "Wrote " <> snapshotFile' <> "."

-- * Parse CLI Options
parseOpts :: IO Options
parseOpts = Optparse.execParser (opts info)
  where
    opts = Optparse.info (optParser <**> Optparse.helper)
    info =
      Optparse.fullDesc
        <> Optparse.progDesc ""
        <> Optparse.header ""

data Options = Options
  { trim :: Bool,
    path :: FilePath
  }
  deriving (Eq, Show)

optParser :: Parser Options
optParser =
  Options <$> optTrim <*> argPath
  where
    optTrim =
      Optparse.switch $
        Optparse.long "trim"
          <> Optparse.short 's'

    argPath =
      Optparse.strArgument $
        Optparse.metavar "FILE"

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
