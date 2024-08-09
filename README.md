# Cardano DB Sync Ledger State Loader

> A simple program that loads cardano-db-sync lstate files into memory

## Building

Build the executable with cabal:

    cabal build exe:lstate-loader

## Usage

    lstate-loader /var/lib/cexplorer/131068792-128d08577d-500.lstate +RTS -l [-hc]
