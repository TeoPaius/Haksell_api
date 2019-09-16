{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Web.Scotty
import Network.HTTP.Types
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative



data User = User String String String -- name pass token
    deriving(Show)

instance FromJSON User where
     parseJSON (Object v) = User <$>
                            v .:  "name"      <*>
                            v .:  "pass"      <*>
                            v .:  "token"
instance ToJSON User where
     toJSON (User name pass token) =
         object ["name" .= name,
                 "pass" .= pass,
                 "token" .= token]

check_user :: User -> Text
check_user (User a b c) = if a == "ok"
                            then "123"
                            else "321"

main = scotty 3000 $ do
  get "/" $ do                         -- handle GET request on "/" URL
    text "This was a GET request!"     -- send 'text/plain' response
  delete "/" $ do
    html "This was a DELETE request!"  -- send 'text/html' response
  post "/" $ do
    text "This was a POST request!"
  put "/" $ do
    text "This was a PUT request!"
  post "/login" $ do
    user <- jsonData :: ActionM User -- Decode body of the POST request as an User object
    token <- return (check_user user)
    text token
