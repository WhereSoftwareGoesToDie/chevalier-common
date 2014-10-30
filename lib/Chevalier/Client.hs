module Chevalier.Client
     ( getSourceDict )
where

import           Control.Monad.Trans
import           Data.Either
import           Data.List
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import qualified System.ZMQ4 as Z

import           Vaultaire.Types
import           Chevalier.Util
import           Chevalier.Types

getSourceDict :: MonadIO m => Origin -> Address -> m (Maybe SourceDict)
getSourceDict org addr = do
  resp <- runChevalier org $ buildRequestFromAddress addr
  return $ fmap snd $ find ((==addr) . fst) resp

runChevalier :: MonadIO m => Origin -> SourceRequest -> m [(Address, SourceDict)]
runChevalier origin req = do
  resp <- liftIO sendrecv
  return $ either (error . show)
                  (rights . map convertSource)
                  (decodeResponse resp)
  where sendrecv =
          Z.withContext          (\ctx  ->
          Z.withSocket ctx Z.Req (\sock -> do
          Z.send sock [Z.SendMore] $ encodeOrigin origin
          Z.send sock []           $ encodeRequest req
          Z.receive sock))
        encodeOrigin (Origin x) = encodeUtf8 $ T.pack $ show x
