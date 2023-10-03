{-|
Module      : GeniusYield.DEX.Api.Constants
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.DEX.Api.Constants ( minDeposit ) where

import GeniusYield.Imports ( Natural )

-- | Altering this constant will result in a modification of the
--   validator address.
minDeposit :: Natural
minDeposit = 2_000_000
