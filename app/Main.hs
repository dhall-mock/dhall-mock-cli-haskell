module Main where

import Control.Lens
import DhallMock.Client
import Network.Wreq as Wreq

main :: IO ()
main = do
    let e = defaultExpectation
          & expRequest.reqMethod .~ (Just GET)
          & expRequest.reqPath .~ (Just $ Path "/foo/bar")
          & expResponse.respStatusCode .~ (Just 200)

    postExpectation defaultClient e
    resp <- Wreq.get "http://localhost:8088/foo/bar"
    print $ resp ^. responseStatus . statusCode

