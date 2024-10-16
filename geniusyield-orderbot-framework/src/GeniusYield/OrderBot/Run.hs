{- |
Module      : GeniusYield.OrderBot.Run
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OrderBot.Run (run) where

import Control.Exception (throwIO)
import GeniusYield.Api.Dex.Constants (
  dexInfoDefaultMainnet,
  dexInfoDefaultPreprod,
 )
import GeniusYield.GYConfig
import GeniusYield.OrderBot (runOrderBot)
import GeniusYield.OrderBot.OrderBotConfig (buildOrderBot, readBotConfig)
import GeniusYield.Types (GYNetworkId (..))
import System.Environment (getArgs)

parseArgs :: IO (String, FilePath, Maybe FilePath)
parseArgs = do
  args <- getArgs
  case args of
    [action, providerConfigFile, botConfigFile] ->
      return
        ( action
        , providerConfigFile
        , Just botConfigFile
        )
    [action, providerConfigFile] -> return (action, providerConfigFile, Nothing)
    _ ->
      throwIO . userError $
        unlines
          [ "Expected two or three command line arguments, in order:"
          , "\t1. Action to execute: 'run'"
          , "\t2. Path to the Atlas provider configuration file"
          , "\t3. Path to the OrderBot config-file (only when reading config from file)"
          ]

run :: IO ()
run = do
  (action, pConfFile, obConfFile) <- parseArgs
  obc <- readBotConfig obConfFile
  cfg <- coreConfigIO pConfFile
  di <-
    case cfgNetworkId cfg of
      GYTestnetPreprod -> pure dexInfoDefaultPreprod
      GYMainnet -> pure dexInfoDefaultMainnet
      _ -> throwIO $ userError "Only Preprod and Mainnet are supported."
  ob <- buildOrderBot obc
  case action of
    "run" -> runOrderBot cfg di ob
    _ -> throwIO . userError $ unwords ["Action: ", show action, " not supported."]
