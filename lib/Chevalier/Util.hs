{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  #-}

module Chevalier.Util where

import qualified Data.ByteString as S
import Data.HashMap.Strict(fromList)
import Data.Locator
import Data.Monoid
import Data.ProtocolBuffers hiding (field)
import Data.Serialize
import Data.Text(splitOn, unpack, append, Text)

import Chevalier.Types
import Vaultaire.Types

-- |Building SourceRequests

buildRequestFromQuery :: SourceQuery -> SourceRequest
buildRequestFromQuery (SourceQuery tags address page page_size _ _) =
    SourceRequest
        { requestTags    = putField tags
        , startPage      = putField $ Just $ fromIntegral page
        , sourcesPerPage = putField $ Just $ fromIntegral page_size
        , addressKey     = putField address'
        }
  where
    address' = case address of
        "*" -> Nothing
        a   -> Just $ fromIntegral $ fromBase62 $ unpack a

-- strict
buildTag :: Text -> Text -> SourceTag
buildTag key value = SourceTag
                     (putField key)
                     (putField value)
buildRequestFromPairs :: [(Text, Text)] -> SourceRequest
buildRequestFromPairs = buildRequestFromTags . map (uncurry buildTag)


-- fuzzy
buildFuzzyRequestTag :: Text -> Text -> SourceTag
buildFuzzyRequestTag key value = SourceTag
                                       (putField key)
                                       (putField $ "*" <> value <> "*")
buildFuzzyRequestFromPairs :: [(Text, Text)] -> SourceRequest
buildFuzzyRequestFromPairs = buildRequestFromTags . map (uncurry buildFuzzyRequestTag)

-- wild
buildWildcardTag :: Text -> SourceTag
buildWildcardTag value = SourceTag
                         (putField "*")
                         (putField $ "*" <> value <> "*")
buildWildRequestFromPairs :: [(Text, Text)] -> SourceRequest
buildWildRequestFromPairs = buildRequestFromTags . map (buildWildcardTag . snd)


wildcardQuery :: [Text] -> SourceRequest
wildcardQuery = buildRequestFromTags . map buildWildcardTag

decodeTag :: SourceTag -> (Text, Text)
decodeTag (SourceTag key value) = (getField key, getField value)

buildRequestFromTags :: [SourceTag] -> SourceRequest
buildRequestFromTags tags =
    SourceRequest (putField tags) (putField Nothing) (putField Nothing) (putField Nothing)

buildRequestFromAddress :: Address -> SourceRequest
buildRequestFromAddress (Address address) =
    SourceRequest (putField []) (putField Nothing) (putField Nothing) (putField $ Just $ fromIntegral $ address)

-- |Converts a Chevalier Source into a Vaultaire (Address, SourceDict) tuple
convertSource :: Source -> Either String (Address, SourceDict)
convertSource Source{..} =
    either
        Left
        (\sd -> Right (Address . fromIntegral $ getField address, sd))
        (makeSourceDict $ fromList $ map decodeTag $ getField tags)

-- |Encodes a SourceRequest into its wire format
encodeRequest :: SourceRequest -> S.ByteString
encodeRequest = runPut . encodeMessage

-- |Decodes a wire format SourceResponse into a list of Sources
decodeResponse :: S.ByteString -> Either ChevalierException [Source]
decodeResponse bs =
    case runGet decodeMessage bs of
        Left  err     -> Left $ BurstDecodeFailure err
        Right decoded ->
            case getField $ chevalierError decoded of
                Just err -> Left  $ ChevalierFailure err
                Nothing  -> Right $ getField (sources decoded)
