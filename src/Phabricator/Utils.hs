{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Phabricator.Utils
    ( apiRequest
    ) where

import Data.Monoid
import Control.Applicative
import Data.Foldable

import Control.Lens
import Network.Wreq
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy as T.L
import qualified Data.IntMap as IM
import qualified Data.HashMap.Strict as HM
import Data.Aeson as Json
import Data.Aeson.Text as Json.Text

import Phabricator.Types

data ConduitResponse a = ConduitResponse { conduitResult :: a
                                         , conduitErrorCode :: Maybe Int
                                         , conduitErrorInfo :: Maybe Value
                                         }

instance FromJSON a => FromJSON (ConduitResponse a) where
    parseJSON = withObject "Conduit response" $ \o ->
      ConduitResponse
        <$> o .: "result"
        <*> o .: "error_code"
        <*> o .: "error_info"

apiRequest :: FromJSON resp
           => Config -> String -> Json.Object -> IO resp
apiRequest (Config phabUrl token) endpoint params = do
    resp <- getWith opts (apiUrl phabUrl endpoint)
    --print (resp ^. responseBody)
    cr <- asJSON resp
    case cr ^. responseBody . to conduitErrorInfo of
      Just info -> fail $ "Conduit error: " <> show info
      Nothing   -> return $ cr ^. responseBody . to conduitResult
  where
    params' :: Json.Object
    params' = params <> ["api.token" Json..= token]

    opts :: Options
    opts = flip appEndo defaults
           $ foldMap (\(path, v) -> Endo $ param (pathToParamKey path) .~ [v])
           $ flattenParams (Json.Object params')

apiUrl :: PhabUrl -> String -> String
apiUrl (PhabUrl base) endpoint = base <> "/api/" <> endpoint

data Path = Field T.Text Path
          | ArrayElement Int Path
          | EmptyPath
          deriving (Show)

newtype PathBuilder = PathBuilder (Path -> Path)

instance Monoid PathBuilder where
    mempty = PathBuilder id
    PathBuilder f `mappend` PathBuilder g = PathBuilder (f . g)

field :: T.Text -> PathBuilder
field = PathBuilder . Field

arrayElement :: Int -> PathBuilder
arrayElement = PathBuilder . ArrayElement

toPath :: PathBuilder -> Path
toPath (PathBuilder f) = f EmptyPath

pathToParamKey :: Path -> T.Text
pathToParamKey path0 =
    case path0 of
      Field field rest -> field <> go rest
      _                -> error "root must be an object"
  where
    go :: Path -> T.Text
    go (Field field rest) =
        mconcat [".", field, go rest]
    go (ArrayElement idx rest) =
        T.concat ["[", T.pack $ show idx, "]", go rest]
    go EmptyPath =
        ""

flattenParams :: Json.Value -> [(Path, T.Text)]
flattenParams val = go mempty val
  where
    go :: PathBuilder -> Value -> [(Path, T.Text)]
    go path (Object obj) =
        foldMap (\(k,v) -> go (path <> field k) v) (HM.toList obj)
    go path (Array obj) =
        foldMap (\(k,v) -> go (path <> arrayElement k) v) (zip [0..] $ toList obj)
    go path other =
        [(toPath path, value)]
      where
        value = case other of
                  Number n   -> T.pack $ show n
                  String t   -> t
                  Bool True  -> "true"
                  Bool False -> "false"
