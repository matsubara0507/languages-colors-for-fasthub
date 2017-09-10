{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           Control.Lens        hiding ((:>))
import           Data.Aeson          hiding (KeyValue)
import           Data.Extensible
import           Data.HashMap.Strict as HM
import           Data.Proxy
import           Data.String         (fromString)
import           Data.Text           (Text)
import           Data.Yaml           as Y
import           GHC.TypeLits        (KnownSymbol, symbolVal)
import           System.Environment  (getArgs)

type GitHubLanguage =
  Record '[
    "type" :> Maybe Text,
    "aliases" :> Maybe [Text],
    "ace_mode" :> Maybe Text,
    "codemirror_mode" :> Maybe Text,
    "wrap" :> Maybe Bool,
    "extensions" :> Maybe [Text],
    "interpreters" :> Maybe [Text],
    "searchable" :> Maybe Bool,
    "language_id" :> Int,
    "color" :> Maybe Text,
    "tm_scope" :> Maybe Text,
    "group" :> Maybe Text
  ]

type GitHubLanguages = HM.HashMap Text GitHubLanguage

type FastHubLanguage =
  Record '[
    "color" :> Maybe Text,
    "url" :> Text
  ]

type FastHubLanguages = HM.HashMap Text FastHubLanguage

main :: IO ()
main = do
  [gfp, ffp] <- getArgs
  glangs <- Y.decodeEither . fromString <$> readFile gfp
  flangs <- eitherDecode . fromString <$> readFile ffp
  either print (mapM_ print . HM.toList) $ diff <$> glangs <*> flangs
  -- print glangs

diff :: GitHubLanguages -> FastHubLanguages -> HM.HashMap Text (GitHubLanguage, FastHubLanguage)
diff glangs flangs = HM.filter (not . uncurry eqColor)
                   . HM.mapMaybeWithKey (\k v -> (,) v <$> HM.lookup k flangs)
                   $ glangs

eqColor :: GitHubLanguage -> FastHubLanguage -> Bool
eqColor glang flang = glang ^. #color == flang ^. #color

type C = KeyValue KnownSymbol FromJSON'

instance Forall C xs => FromJSON (Record xs) where
  parseJSON = withObject "Object" $
    \v -> hgenerateFor (Proxy :: Proxy C) $
    \m -> let k = symbolVal (proxyAssocKey m) in
      case HM.lookup (fromString k) v of
        Just a  -> Field . return <$> parseJSON a
        Nothing ->
          maybe (fail $ "Missing key: " `mappend` k)
            (fmap (Field . return)) $ parseJSON' Nothing

class FromJSON a => FromJSON' a where
  parseJSON' :: Maybe Value -> Maybe (Parser a)

instance FromJSON a => FromJSON' a where
  parseJSON' = fmap parseJSON

instance {-# OVERLAPS #-} FromJSON a => FromJSON' (Maybe a) where
  parseJSON' = Just . maybe (pure Nothing) parseJSON
