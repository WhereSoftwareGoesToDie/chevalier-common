module Chevalier.Client
     ( getSourceDict, getAddresses )
where

import           Control.Monad.Trans
import           Data.Either
import           Data.List
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

getAddresses :: MonadIO m => URI -> Origin -> (String, String) -> m [(Address, SourceDict)]
getAddresses uri org (k,v) =
  runChevalier uri org $ buildRequestFromPairs [(T.pack k, T.pack v)]

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
