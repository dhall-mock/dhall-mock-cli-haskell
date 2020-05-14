module Main where

import Control.Lens
import DhallMockApi
import Network.Wreq as Wreq

main :: IO ()
main = do
    let e = defaultExpectation
          & expRequest.reqMethod .~ (Just GET)
          & expRequest.reqPath .~ (Just $ Path "/foo/bar")
          & expResponse.respStatusCode .~ (Just 200)

    postExpectation e
    resp <- Wreq.get "http://localhost:8088/foo/bar"
    print $ resp ^. responseStatus . statusCode

