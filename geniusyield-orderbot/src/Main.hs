{-|
Module      : Main
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module Main ( main ) where

import Control.Exception ( throwIO )
import System.Environment (getArgs)
import GeniusYield.OrderBot ( runOrderBot )
import OrderBotConfig ( readBotConfig, buildOrderBot, getDexInfo )

parseArgs :: IO (String, FilePath, Maybe FilePath)
parseArgs = do
    args <- getArgs
    case args of
        [action, providerConfigFile,botConfigFile] -> return ( action
                                                             , providerConfigFile
                                                             , Just botConfigFile
                                                             )
        [action, providerConfigFile] -> return (action, providerConfigFile, Nothing)
        _ -> throwIO . userError $ unlines
             [ "Expected two or three command line arguments, in order:"
             , "\t1. Action to execute: 'run'"
             , "\t2. Path to the Atlas provider configuration file"
             , "\t3. Path to the OrderBot config-file (only when reading config from file)"
             ]

main :: IO ()
main = do
    (action, pConfFile,obConfFile) <- parseArgs
    obc <- readBotConfig obConfFile
    di <- getDexInfo obc
    ob <- buildOrderBot obc
    case action of
        "run" -> runOrderBot pConfFile di ob
        _ -> throwIO . userError $ unwords ["Action: ", show action, " not supported."]
