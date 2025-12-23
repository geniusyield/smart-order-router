{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GeniusYield.OrderBot.Oracle (OracleCertificate (..), Price (..), getOracleCertificate, extractPriceFromCert) where

-- import Control.Monad (void)

-- import Data.ByteString (ByteString)

-- import Network.HTTP.Req (GET (..), NoReqBody (..), req, responseBody, runReq, defaultHttpConfig, https, (/:), ReqBodyJson(..), POST(..), jsonResponse)

-- import Data.Char.ByteString.Char8 as BS
-- import qualified Data.ByteString as BS

-- import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
-- import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.Text (Text, unpack)
-- import Data.Text.Encoding (decodeUtf8)
import Data.Time
-- import Data.Time.Clock (UTCTime)
-- import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format.ISO8601
-- import Debug.Trace (trace)
import GHC.Generics (Generic)
import GeniusYield.Api.Oracle (OracleCertificate (..), Price (..), SignatureOffchain (..))
import GeniusYield.Types
import GeniusYield.Types.Value ()
import Network.HTTP.Req (POST (..), ReqBodyJson (..), defaultHttpConfig, http, jsonResponse, port, req, responseBody, runReq, (/:))

parseTimestamp :: String -> Maybe UTCTime
parseTimestamp = iso8601ParseM

data PriceFraction = PriceFraction {pfNumerator :: !Integer, pfDenominator :: !Integer}
  deriving stock (Generic, Show)

instance Aeson.FromJSON PriceFraction where
  parseJSON = Aeson.withObject "PriceFraction" $ \v -> PriceFraction <$> v Aeson..: "numerator" <*> v Aeson..: "denominator"

instance Aeson.ToJSON PriceFraction where
  toJSON PriceFraction {..} = Aeson.object ["pfNumerator" Aeson..= pfNumerator, "pfDenominator" Aeson..= pfDenominator]

priceFractionToPrice :: PriceFraction -> Price
priceFractionToPrice pf = Price (pfNumerator pf % pfDenominator pf)

data OracleCertificateRequest = OracleCertificateRequest
  { ocrReqBaseSymbol :: !Text
  , ocrReqBaseToken :: !Text
  , ocrReqQuoteSymbol :: !Text
  , ocrReqQuoteToken :: !Text
  }
  deriving stock (Generic, Show)

instance Aeson.ToJSON OracleCertificateRequest where
  toJSON OracleCertificateRequest {..} =
    Aeson.object
      [ "baseSymbol" Aeson..= ocrReqBaseSymbol
      , "baseToken" Aeson..= ocrReqBaseToken
      , "quoteSymbol" Aeson..= ocrReqQuoteSymbol
      , "quoteToken" Aeson..= ocrReqQuoteToken
      ]

data OracleCertificateResponse = OracleCertificateResponse
  { ocrResStatus :: !Text
  , ocrResFailedSources :: !(Maybe [Text])
  , ocrResBaseSymbol :: !Text
  , ocrResBaseToken :: !Text
  , ocrResQuoteSymbol :: !Text
  , ocrResQuoteToken :: !Text
  , ocrResPrice :: !PriceFraction
  , ocrResTimestamp :: !Text
  , ocrResSignature :: !SignatureOffchain
  }
  deriving stock (Generic, Show)

instance Aeson.FromJSON OracleCertificateResponse where
  parseJSON = Aeson.withObject "OracleCertificateResponse" $ \v ->
    OracleCertificateResponse
      <$> v Aeson..: "status"
      <*> v Aeson..: "failedSources"
      <*> v Aeson..: "baseSymbol"
      <*> v Aeson..: "baseToken"
      <*> v Aeson..: "quoteSymbol"
      <*> v Aeson..: "quoteToken"
      <*> v Aeson..: "price"
      <*> v Aeson..: "timestamp"
      <*> v Aeson..: "signature"

renderAssetClass :: GYAssetClass -> (Text, Text)
renderAssetClass GYLovelace = ("", "")
renderAssetClass (GYToken policyId tokenName) = (mintingPolicyIdToText policyId, tokenNameToHex tokenName)

parseAssetClass :: (Text, Text) -> Maybe GYAssetClass
parseAssetClass ("", "") = Just GYLovelace
parseAssetClass (policy, token) =
  GYToken
    <$> (eitherToMaybe $ mintingPolicyIdFromText policy)
    <*> (eitherToMaybe $ tokenNameFromHex token)
 where
  eitherToMaybe (Left _) = Nothing
  eitherToMaybe (Right x) = Just x

responseToCertificate :: OracleCertificateResponse -> Maybe OracleCertificate
responseToCertificate res = do
  mBase <- parseAssetClass (ocrResBaseSymbol res, ocrResBaseToken res)
  mQuote <- parseAssetClass (ocrResQuoteSymbol res, ocrResQuoteToken res)
  mTime <- iso8601ParseM (unpack (ocrResTimestamp res))
  return $
    OracleCertificate
      { ocPrice = priceFractionToPrice (ocrResPrice res)
      , ocBaseAsset = mBase
      , ocQuoteAsset = mQuote
      , ocTimestamp = mTime
      , ocSignature = ocrResSignature res
      }

getOracleCertificate :: (GYAssetClass, GYAssetClass) -> IO (Maybe OracleCertificate)
getOracleCertificate (baseAsset, quoteAsset) = do
  let (baseSymbol, baseToken) = renderAssetClass baseAsset
      (quoteSymbol, quoteToken) = renderAssetClass quoteAsset
      requestBody =
        OracleCertificateRequest
          { ocrReqBaseSymbol = baseSymbol
          , ocrReqBaseToken = baseToken
          , ocrReqQuoteSymbol = quoteSymbol
          , ocrReqQuoteToken = quoteToken
          }
  runReq defaultHttpConfig $ do
    response <- req POST (http "localhost" /: "DEX" /: "two-way-order" /: "oracle-certificate") (ReqBodyJson requestBody) jsonResponse (port 8082)
    let oracleResponse = responseBody response
    return $ responseToCertificate oracleResponse

extractPriceFromCert :: OracleCertificate -> Rational
extractPriceFromCert = getPrice . ocPrice
