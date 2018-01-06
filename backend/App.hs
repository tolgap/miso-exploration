{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module App where

import           ApiType                      (EntryAPI, StaticAPI)
import           Control.Monad.IO.Class
import           Control.Monad.Logger         (NoLoggingT, runStderrLoggingT)
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Reader   (ReaderT)
import           Control.Monad.Trans.Resource (MonadBaseControl, ResourceT)
import           Data.Pool                    (Pool)
import           Data.Proxy
import           Data.Text                    as T
import           Database.Persist
import           Database.Persist.Sql         hiding (SqlBackend)
import           Database.Persist.Sqlite
import           Foundation
import qualified Lucid                        as L
import qualified Lucid.Base                   as L
import           Miso                         (ToServerRoutes)
import qualified Miso.String                  as S
import           Models
import           Network.Wai
import           Network.Wai.Handler.Warp     as Warp
import           Servant
import           Servant.HTML.Lucid

newtype HtmlPage a = HtmlPage a
    deriving (Show, Eq)

type ServerAPI =
    ServerRoutes :<|> StaticAPI :<|> EntryAPI

type ServerRoutes =
    ToServerRoutes ClientRoutes HtmlPage Msg

instance L.ToHtml a => L.ToHtml (HtmlPage a) where
    toHtmlRaw = L.toHtml
    toHtml (HtmlPage x) =
        L.doctypehtml_ $ do
            L.head_ $ do
                L.title_ "Miso isomorphic"
                L.meta_ [L.charset_ "utf-8"]

                L.with (L.script_ mempty)
                    [ L.makeAttribute "src" "static/all.js"
                    , L.makeAttribute "async" mempty
                    , L.makeAttribute "defer" mempty
                    ]
                L.with (L.link_ mempty)
                    [ L.makeAttribute "href" "static/style.css"
                    , L.makeAttribute "rel" "stylesheet"
                    ]

            L.body_ (L.toHtml x)

getDBPool :: IO (Pool SqlBackend)
getDBPool =
    runStderrLoggingT $ createSqlitePool "db/miso-test.sqlite3" 1


runDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runDB query = do
    pool <- getDBPool
    runSqlPersistMPool query pool

dbHandler :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> ExceptT ServantErr IO a
dbHandler =
    liftIO  . runDB

app =
    serve (Proxy @ServerAPI) (serverRouteHandlers :<|> static :<|> entryHandlers)
    where
        static =
            serveDirectory "backend/static"

        serverRouteHandlers = do
            entries <- getEntries
            return $ HtmlPage $ viewModel $ initialModel entries

        entryHandlers =
            getEntries :<|> storeEntry :<|> updateEntry :<|> deleteEntry

storeEntry :: Entry -> ExceptT ServantErr IO ()
storeEntry Entry{..} =
    dbHandler $ do
        insert $ DbEntry description completed editing eid focussed
        return ()

updateEntry :: Int -> Entry -> ExceptT ServantErr IO ()
updateEntry entryId Entry{..} =
    dbHandler $ do
        updateWhere [DbEntryEid ==. entryId]
            [ DbEntryDescription =. T.pack (S.unpack description)
            , DbEntryCompleted =. completed
            , DbEntryEditing =. editing
            , DbEntryFocussed =. focussed
            ]
        return ()

deleteEntry :: Int -> ExceptT ServantErr IO ()
deleteEntry entryId =
    dbHandler $ do
        deleteWhere [DbEntryEid ==. entryId]
        return ()

getEntries :: ExceptT ServantErr IO [Entry]
getEntries =
    dbHandler $ do
        entries <- selectList [] []
        return $ Prelude.map (\(Entity _ e) -> entityToEntry e) entries

mkApp = do
    runDB (runMigration migrateAll)
    return app

run :: IO ()
run =
    Warp.run 3000 =<< mkApp

entityToEntry DbEntry{..} =
    Entry {
        description = dbEntryDescription,
        completed = dbEntryCompleted,
        editing = dbEntryEditing,
        eid = dbEntryEid,
        focussed = dbEntryFocussed
    }
