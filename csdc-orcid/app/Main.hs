{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import CSDC.ORCID

import Data.Aeson (encode, decodeFileStrict)
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)

import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main =
  decodeFileStrict "orcid-config.json" >>= \case
    Nothing -> error "Could not decode orcid-config.json file."
    Just config -> do
      let
        application request respond = do
          token <- getToken request
          record <- getUserRecord Production (token_access token) (token_orcid token)
          let
            response = case record of
              Left e -> e
              Right o -> encode o
          respond $ responseLBS status200 [] response

      putStrLn "Starting."
      middleware <- authenticationMiddleware config
      Warp.run 3000 $ middleware application
