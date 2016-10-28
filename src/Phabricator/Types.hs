{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Phabricator.Types where

import Data.Aeson
import Data.Text (Text)

-- | Base URL of a Phabricator instance.
newtype PhabUrl = PhabUrl String
                deriving (Show)

-- | An API token
newtype ApiToken = ApiToken String
                 deriving (Show, FromJSON, ToJSON)

-- | API usage configuration
data Config = Config PhabUrl ApiToken
            deriving (Show)

newtype Phid = Phid Text
             deriving (Show, FromJSON, ToJSON)

newtype CursorResponse a = CursorResponse { cursorResponseData :: a }
                          deriving (Show)

instance FromJSON a => FromJSON (CursorResponse a) where
    parseJSON = withObject "Cursor response" $ \o ->
      CursorResponse <$> o .: "data"
