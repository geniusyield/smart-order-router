module GeniusYield.OrderBot.DataSource.Arbitrary (
  Connection,
  connectDB,
  closeDB,
  withEachAssetOrders,
) where

import qualified Cardano.Api as Api
import Data.Ratio ((%))
import GeniusYield.OrderBot.Types
import GeniusYield.Types
import Hedgehog
import Hedgehog.Gen hiding (constant)
import Hedgehog.Range
import qualified Test.Gen.Cardano.Api.Typed as ApiGen

type Connection = ()

type Scripts = ()

type OrderData = (# OrderAssetPair, [OrderInfo 'BuyOrder], [OrderInfo 'SellOrder] #)

gyTxOutRef :: Gen GYTxOutRef
gyTxOutRef = txOutRefFromApi <$> ApiGen.genTxIn

-- not sure if this should be a UTF8 encoded bs?
gyTokenName :: Gen GYTokenName
gyTokenName = (\(Api.AssetName x) -> GYTokenName x) <$> ApiGen.genAssetName

gyMintingPolicyId :: Gen GYMintingPolicyId
gyMintingPolicyId = mintingPolicyIdFromApi . Api.PolicyId <$> ApiGen.genScriptHash

orderAssetPair :: Gen OrderAssetPair
orderAssetPair =
  mkOrderAssetPair GYLovelace
    <$> (GYToken <$> gyMintingPolicyId <*> gyTokenName)

genVolume :: Gen Volume
genVolume = do
  (vl :: Natural) <- integral (constant 1 1000 :: Range Natural)
  vh <- integral (constant vl 1100 :: Range Natural)
  pure $ Volume vl vh

genPrice :: Gen Price
genPrice = fmap Price $ (%) <$> (fromIntegral <$> go) <*> (fromIntegral <$> go)
 where
  go :: Gen Natural
  go = integral (constant 1 2)

listRange :: Range Int
listRange = constant 20 30

sampleList :: Gen a -> IO [a]
sampleList = sample . list listRange

connectDB :: GYNetworkId -> GYProviders -> IO Connection
connectDB _ _ = pure ()

closeDB :: Connection -> IO ()
closeDB _ = pure ()

-- Note: only generates one asset pair for now.
withEachAssetOrders :: Connection -> Scripts -> (a -> OrderData -> a) -> a -> IO a
withEachAssetOrders _ _ f acc = do
  oap <- sample orderAssetPair
  buyOrders <-
    sampleList $
      OrderInfo <$> pure SBuyOrder <*> gyTxOutRef <*> pure oap <*> genVolume <*> pure 0 <*> genPrice <*> pure Nothing <*> pure Nothing <*> pure Nothing
  sellOrders <-
    sampleList $
      OrderInfo <$> pure SSellOrder <*> gyTxOutRef <*> pure oap <*> genVolume <*> pure 0 <*> genPrice <*> pure Nothing <*> pure Nothing <*> pure Nothing
  pure $ f acc (# oap, buyOrders, sellOrders #)
