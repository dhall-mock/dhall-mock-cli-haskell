module DhallMockCommand where

import Control.Lens
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import DhallMock.Client
import qualified Data.CaseInsensitive as CI
import Data.Typeable (Typeable)
import Network.Wreq (Options)
import qualified Network.Wreq as Wreq
import Network.HTTP.Client ( defaultManagerSettings
                           , managerResponseTimeout
                           , responseTimeoutMicro )

import Hedgehog (Command(..), MonadGen, MonadTest, Callback(..), HTraversable(..), (/==))
import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range

methodGen :: MonadGen m => m HttpMethod
methodGen =
    Gen.element [ GET
                , POST
                , PUT
                , PATCH
                , DELETE
                ]

pathGen :: MonadGen m => m Path
pathGen = do
    let genPathChunk = Gen.text (Range.linear 10 20) Gen.alphaNum

    numChunk <- Gen.integral (Range.linear 4 10)

    fmap (Path . ((<>) "/") . T.intercalate "/") $ replicateM numChunk genPathChunk

bodyGen :: MonadGen m => m Body
bodyGen = Gen.choice [ fmap TEXT $ Gen.text (Range.linear 0 999) Gen.alphaNum
                     , pure (JSON $ JsonValue $ Aeson.Null) --TODO
                     ]

paramsGen :: MonadGen m => m QueryParams
paramsGen = do
    let genText = Gen.text (Range.linear 1 10) Gen.alphaNum
        genKv   = (,) <$> genText <*> genText

    numParams <- Gen.integral (Range.linear 0 50)

    fmap QueryParams $ replicateM numParams genKv

headersGen :: MonadGen m => m Headers
headersGen = do
    let genText = Gen.text (Range.linear 1 10) Gen.alphaNum
        genKv   = (,) <$> genText <*> genText

    numHeaders <- Gen.integral (Range.linear 0 50)

    fmap (Headers . Map.fromList) $ replicateM numHeaders genKv

reqSpecGen :: MonadGen m => m ReqSpec
reqSpecGen =
    ReqSpec <$> Gen.choice [ pure Nothing, fmap Just methodGen ]
            <*> Gen.choice [ pure Nothing, fmap Just pathGen   ]
            <*> Gen.choice [ pure Nothing, fmap Just bodyGen   ]
            <*> paramsGen
            <*> headersGen

respSpecGen :: MonadGen m => m RespSpec
respSpecGen =
    RespSpec <$> pure (Just 200) --TODO
             <*> pure Nothing --TODO
             <*> Gen.choice [ pure Nothing ] --TODO
             <*> headersGen

expectationGen :: MonadGen m => m Expectation
expectationGen = Expectation <$> reqSpecGen <*> respSpecGen

chooseExpectationGen :: MonadGen m => [Expectation] -> m Expectation
chooseExpectationGen = Gen.element

newtype RegisterExpectation (v :: * -> *) =
    RegisterExpectation Expectation deriving (Eq, Show, Typeable)

instance HTraversable RegisterExpectation where
    htraverse _ (RegisterExpectation c) = pure (RegisterExpectation c)

data SubmitRequest (v :: * -> *) =
    SubmitRequest { _submitRequestExpectation :: Expectation
                  , _submitRequestUrl         :: Text
                  , _submitRequestOpts        :: Options
                  , _submitRequestMethod      :: HttpMethod
                  , _submitRequestBody        :: ByteString
                  } deriving (Show)

instance HTraversable SubmitRequest where
    htraverse _ (SubmitRequest e url opts m b) = pure (SubmitRequest e url opts m b)

newtype MockState (v :: * -> *) =
    MockState { _mockStateCatalog :: [Expectation]
              } deriving (Eq, Show, Typeable)

registerExpectationCmd :: (MonadGen gen, MonadIO m, MonadTest m)
                       => Command gen m MockState
registerExpectationCmd =
    let gen (MockState _) = Just $ fmap RegisterExpectation expectationGen
        execute (RegisterExpectation e) = liftIO $ do
            print ("register request..." :: String)
            postExpectation e
     in Command gen execute [
            Update $ \(MockState catalog) (RegisterExpectation e) _rsp ->
                MockState (e:catalog)
        ]

sendRandomRequestCmd :: (MonadGen gen, MonadIO m, MonadTest m)
                     => Command gen m MockState
sendRandomRequestCmd =
    Command gen execute [
        Ensure $ \_ _ _ code -> code /== 404
    ]
    where
        gen (MockState expectations) =
            if null expectations
               then Nothing
               else Just $ toSubmitRequest =<< Gen.element expectations

        execute (SubmitRequest _ p opts m b) = liftIO $ do
            print ("submit request..." :: String)
            resp <- case m of
              GET    -> Wreq.customPayloadMethodWith "GET" opts (T.unpack p) b
              POST   -> Wreq.customPayloadMethodWith "POST" opts (T.unpack p) b
              PUT    -> Wreq.customPayloadMethodWith "PUT" opts (T.unpack p) b
              PATCH  -> Wreq.customPayloadMethodWith "PATCH" opts (T.unpack p) b
              DELETE -> Wreq.customPayloadMethodWith "DELETE" opts (T.unpack p) b
            pure (resp ^. Wreq.responseStatus.Wreq.statusCode)

        mkPath e =
            case e ^. expRequest.reqPath of
              Just p  -> pure (_getPath p)
              Nothing -> fmap _getPath pathGen

        mkOpts e = do
            let h  = e ^. expRequest.reqHeaders
            h' <- headersGen
            let headers = fmap (bimap (CI.mk . encodeUtf8) encodeUtf8)
                        $ Map.toList
                        $ _getHeaders
                        $ h' <> h

            let p = e ^. expRequest.reqParams
            p' <- paramsGen
            let params = _getParams $ p' <> p

            pure $ Wreq.defaults
                 & Wreq.params %~ ((<>) params)
                 & Wreq.headers %~ ((<>) headers)
                 & Wreq.checkResponse .~ Nothing
                 & Wreq.manager .~ Left (defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 10000 } )

        mkMethod e =
            case e ^. expRequest.reqMethod of
              Just m  -> pure m
              Nothing -> methodGen

        mkBody e = do
            b <- case e ^. expRequest.reqBody of
                   Just b  -> pure b
                   Nothing -> bodyGen
            pure $ case b of
                     JSON j -> BSL.toStrict $ Aeson.encode $ _getJsonValue j
                     TEXT t -> encodeUtf8 t

        toSubmitRequest e =
            SubmitRequest e <$> (fmap ((<>) "http://localhost:8088") $ mkPath e)
                            <*> mkOpts e
                            <*> mkMethod e
                            <*> mkBody e
