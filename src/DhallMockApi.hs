{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module DhallMockApi where

import Control.Lens
import qualified Data.Aeson as Json
import Data.Aeson.Text (encodeToLazyText)
import Data.Char (toLower)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

import Network.Wreq as Wreq

import Dhall (ToDhall(..), InterpretOptions(..), defaultInterpretOptions, genericToDhallWith)
import qualified Dhall as Dhall
import qualified Dhall.Core as DhallC

dhallStripPrefix :: Text -> InterpretOptions
dhallStripPrefix p = defaultInterpretOptions { fieldModifier = f }
    where
        f field =
            let field' = fromMaybe field $ T.stripPrefix p field
             in T.cons (toLower . T.head $ field') (T.tail field')

data HttpMethod =
        GET
      | POST
      | PUT
      | PATCH
      | DELETE deriving (Eq, Show, Generic)

instance ToDhall HttpMethod

newtype Path = Path { _getPath :: Text
                    } deriving (Eq, Show, Generic)

instance ToDhall Path where
    injectWith opts = contramap _getPath (injectWith opts)

newtype JsonValue =
    JsonValue { _getJsonValue :: Json.Value
              } deriving (Eq, Show)

instance ToDhall JsonValue where
    injectWith opts = contramap (encodeToLazyText . _getJsonValue) (injectWith opts)

data Body =
        JSON { json :: JsonValue }
      | TEXT { text :: Text }
      deriving (Eq, Show, Generic)

instance ToDhall Body

data QueryParams =
    QueryParams { _getParams :: [(Text, Text)]
                }
                deriving (Eq, Show, Generic)

instance Semigroup QueryParams where
    a <> b = QueryParams $ _getParams a <> _getParams b

instance Monoid QueryParams where
    mempty = QueryParams []

instance ToDhall QueryParams where
    injectWith opts = contramap _getParams (injectWith opts)

data Headers =
    Headers { _getHeaders :: Map Text Text
            } deriving (Eq, Show)

instance Semigroup Headers where
    a <> b = Headers $ _getHeaders a <> _getHeaders b

instance Monoid Headers where
    mempty = Headers Map.empty

instance ToDhall Headers where
    injectWith opts = contramap _getHeaders (injectWith opts)

data ReqSpec =
    ReqSpec { _reqMethod  :: Maybe HttpMethod
            , _reqPath    :: Maybe Path
            , _reqBody    :: Maybe Body
            , _reqParams  :: QueryParams
            , _reqHeaders :: Headers
            } deriving (Eq, Show, Generic)

instance ToDhall ReqSpec where
    injectWith _ = genericToDhallWith $ dhallStripPrefix "_req"

makeLenses ''ReqSpec

data RespSpec =
    RespSpec { _respStatusCode   :: Maybe Int
             , _respStatusReason :: Maybe Text
             , _respBody         :: Maybe Text
             , _respHeaders      :: Headers
             } deriving (Eq, Show, Generic)

instance ToDhall RespSpec where
    injectWith _ = genericToDhallWith $ dhallStripPrefix "_resp"

makeLenses ''RespSpec

data Expectation =
    Expectation { _expRequest  :: ReqSpec
                , _expResponse :: RespSpec
                } deriving (Eq, Show, Generic)

instance ToDhall Expectation where
    injectWith _ = genericToDhallWith $ dhallStripPrefix "_exp"

makeLenses ''Expectation

defaultExpectation :: Expectation
defaultExpectation =
    let req = ReqSpec { _reqMethod  = Nothing
                      , _reqPath    = Nothing
                      , _reqBody    = Nothing
                      , _reqParams  = mempty
                      , _reqHeaders = mempty
                      }
        resp = RespSpec { _respStatusCode   = Nothing
                        , _respStatusReason = Nothing
                        , _respBody         = Nothing
                        , _respHeaders      = mempty
                        }
     in Expectation req resp

postExpectation :: Expectation -> IO ()
postExpectation expectation = do
    let payload = DhallC.pretty $ Dhall.embed Dhall.inject [expectation]
    _ <- Wreq.post "http://localhost:8089/expectations" (encodeUtf8 payload)
    return ()
