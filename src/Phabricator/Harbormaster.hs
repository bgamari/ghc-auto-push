{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Phabricator.Harbormaster where

import Data.Aeson
import Control.Applicative

import Phabricator.Types
import Phabricator.Utils

data BuildableStatus = Failed
                     deriving (Show)

instance FromJSON BuildableStatus where
    parseJSON = withText "buildable status" $ \s ->
      case s of
        "failed" -> pure Failed
        _        -> empty

data Buildable = Buildable { buildablePhid :: Phid
                           , buildableStatus :: BuildableStatus
                           , buildableContainerPhid :: Phid
                           }
               deriving (Show)

instance FromJSON Buildable where
    parseJSON = withObject "buildable" $ \o ->
      Buildable
        <$> o .: "buildablePHID"
        <*> o .: "buildableStatus"
        <*> o .: "containerPHID"

-- | Query buildable statuses by containers 'Phid'.
queryBuildables :: Config
                -> [Phid]  -- ^ 'Phid's of containers
                -> IO [Buildable]
queryBuildables config containers =
    fmap cursorResponseData
    $ apiRequest config "harbormaster.querybuildables"
      ["containerPHIDs" .= containers ]
