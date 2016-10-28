{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Phabricator.Phid
    ( lookupPhids
    ) where

import Data.Text (Text)
import qualified Data.Map as M
import Data.Aeson

import Phabricator.Types
import Phabricator.Utils

data LookupResponse = LookupResponse { phid :: Phid
                                     }
                    deriving (Show)

instance FromJSON LookupResponse where
    parseJSON = withObject "PHID lookup response" $ \o ->
      LookupResponse
        <$> o .: "phid"

lookupPhids :: Config -> [Text] -> IO (M.Map Text Phid)
lookupPhids config names =
    fmap phid
    <$> apiRequest config "phid.lookup" ["names" .= names]
