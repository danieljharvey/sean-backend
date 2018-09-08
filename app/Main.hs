{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson              hiding (json)
import           Data.Monoid             ((<>))
import           Data.Text               (Text, pack)
import           Prelude

import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist        hiding (get)
import qualified Database.Persist        as P
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

Story json -- The json keyword will make Persistent generate sensible ToJSON and FromJSON instances for us.
  json Text
  deriving Show
|]

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

main :: IO ()
main = do
  print "Starting Sean's Big Backend"
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- (\cfg -> cfg { spc_csrfProtection = True }) <$> defaultSpockCfg () (PCPool pool) ()
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  runSpock 8080 (spock spockCfg app)

app :: Api
app = do
  get "stories" $ do
    allStories <- runSQL $ selectList [] [Asc StoryId]
    json allStories
  post "stories" $ do
    maybeStory <- jsonBody :: ApiAction (Maybe Story)
    case maybeStory of
      Nothing -> errorJson 1 "Failed to parse request body as Story"
      Just theStory -> do
        newId <- runSQL $ insert theStory
        json $ object ["result" .= String "success", "id" .= newId]
  get ("stories" <//> var) $ \storyId -> do
    maybeStory <- runSQL $ P.get storyId :: ApiAction (Maybe Story)
    case maybeStory of
      Nothing       -> errorJson 2 "Could not find a story with matching id"
      Just theStory -> json theStory

runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
  object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]
