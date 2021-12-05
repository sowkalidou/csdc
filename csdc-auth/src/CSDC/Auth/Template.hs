{-# LANGUAGE QuasiQuotes #-}

module CSDC.Auth.Template
  ( loginTemplate
  ) where

import Data.ByteString.Builder (Builder)
import Data.Text (Text)
import Network.Wai.Middleware.Auth.Provider
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Text.Hamlet (Render, hamlet)

loginTemplate ::
     Maybe Text -- ^ Error message to display, if any.
  -> Render Provider -- ^ Renderer function for provider urls.
  -> Providers -- ^ List of available providers.
  -> Builder
loginTemplate merrMsg render providers =
  renderHtmlBuilder $ [hamlet|
$doctype 5
<html>
  <head>
    <title>CSDC DAO</title>
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bulma@0.9.3/css/bulma.min.css"/>
  <body>
    <div .media-container>
      <h3>Select one of available authentication methods:
      $maybe errMsg <- merrMsg
        <div .alert .alert-danger role="alert">
          #{errMsg}
      $forall provider <- providers
        $with info <- getProviderInfo provider
          <div .media.provider>
            <a href=@{provider}>
              <div .media-left .container>
                <img .provider-logo src=#{providerLogoUrl info}>
              <div .media-body>
                <h3 .media-heading>
                  #{providerTitle info}
                #{providerDescr info}
|] render

