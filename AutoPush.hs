{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Monoid
import Control.Applicative
import Data.Foldable

import qualified Data.Text as T

import Options.Applicative

import Phabricator.Types
import Phabricator.Phid
import Phabricator.Harbormaster

haskellPhab :: PhabUrl
haskellPhab = PhabUrl "https://phabricator.haskell.org"

parseApiToken :: Parser ApiToken
parseApiToken =
    option (ApiToken <$> str) (long "api-token" <> short 'a' <> help "Conduit API token")

main :: IO ()
main = do
    apiToken <- execParser $ info (helper <*> parseApiToken) mempty
    let config = Config haskellPhab apiToken
    phids <- lookupPhids config ["D2643"]
    print phids
    buildables <- queryBuildables config (toList phids)
    print buildables
