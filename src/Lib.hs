{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Network.HTTP.Client      (newManager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Web.Telegram.API.Bot
import Data.Text

sayToChannel :: String -> String -> ReaderT String IO ()
sayToChannel channel message = do
    token <- Token . pack <$> ask
    settings <- liftIO $ newManager tlsManagerSettings

    let request = sendMessageRequest (pack channel) (pack message)
    liftIO $ sendMessage token request settings >>= print

run = flip runReaderT
