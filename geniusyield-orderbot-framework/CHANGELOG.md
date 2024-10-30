# Revision history for geniusyield-orderbot-framework

## 0.5.1

* Additional configuration parameter, `lovelaceWarningThreshold` which denotes lovelace value. If bot's balance is below this threshold, log with warning severity is made.
* Support for price providers to assess profitability, see details in updated README file.

## 0.5.0

Adds strategy signature, utilities to different orderbook, etc. signatures and more modules related to order bot configuration and command line parsing.

## 0.4.0

Conway era support. Note that this update is not compatible with Babbage era and so must be employed on Mainnet after Chang HF.

## 0.3.0

Update Maestro SDK to not make use of deprecated endpoints.

## 0.2.0

Incorporate v0.3.1.0 of `geniusyield-dex-api`.