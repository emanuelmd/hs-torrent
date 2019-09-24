module HTTPClient where

import           Prelude

import           Network.HTTP.Client (defaultManagerSettings, httpLbs,
                                      newManager, parseRequest, responseBody)

type URL = Text

fetchUrl :: URL -> IO Text
fetchUrl url = do
  manager <- newManager defaultManagerSettings

  request <- parseRequest $ toS url
  response <- httpLbs request manager

  return $ (toS . responseBody) response


