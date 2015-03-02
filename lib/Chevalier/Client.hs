module Chevalier.Client(
    getSourceDict,
    getAddresses,
    getAddresses'
) where

import           Control.Monad.Trans
import           Data.Bifunctor
import           Data.Either
import           Data.List
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import           Network.URI
import qualified System.ZMQ4 as Z

import           Vaultaire.Types
import           Chevalier.Util
import           Chevalier.Types

getSourceDict :: MonadIO m => URI -> Origin -> Address -> m (Maybe SourceDict)
getSourceDict uri org addr = do
  resp <- runChevalier uri org $ buildRequestFromAddress addr
  return $ fmap snd $ find ((==addr) . fst) resp

-- | Take one key-value pair and return matched addresses and
--   sourcedicts.
getAddresses :: MonadIO m => URI -> Origin -> (String, String) -> m [(Address, SourceDict)]
getAddresses uri org = getAddresses' uri org . (: [])

-- | Take multiple key-value pairs and return matchec addresses and
--   sourcedicts.
getAddresses' :: MonadIO m => URI -> Origin -> [(String, String)] -> m [(Address, SourceDict)]
getAddresses' uri org tags =
  runChevalier uri org $ buildRequestFromPairs $ packTags tags
 where
  packTags = map (bimap T.pack T.pack)

runChevalier :: MonadIO m => URI -> Origin -> SourceRequest -> m [(Address, SourceDict)]
runChevalier uri origin req = do
  resp <- liftIO sendrecv
  return $ either (error . show)
                  (rights . map convertSource)
                  (decodeResponse resp)
  where sendrecv =
          Z.withContext          (\ctx  ->
          Z.withSocket ctx Z.Req (\sock -> do
          Z.connect sock $ show uri
          Z.send sock [Z.SendMore] $ encodeOrigin origin
          Z.send sock []           $ encodeRequest req
          Z.receive sock))
        encodeOrigin (Origin x) = encodeUtf8 $ T.pack $ show x
