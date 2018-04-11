{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module App where

import           ApiType                              (EntryAPI, StaticAPI)
import           Control.Monad.IO.Class
import           Control.Monad.Logger                 (NoLoggingT,
                                                       runStderrLoggingT)
import           Control.Monad.Trans.Except           (ExceptT)
import           Control.Monad.Trans.Reader           (ReaderT)
import           Control.Monad.Trans.Resource         (ResourceT)
import           Data.Pool                            (Pool)
import           Data.Proxy
import           Data.Text                            as T
import           Database.Persist
import           Database.Persist.Sql                 hiding (SqlBackend)
import           Database.Persist.Sqlite
import           Foundation
import qualified Lucid                                as L
import qualified Lucid.Base                           as L
import           Miso                                 (ToServerRoutes, View)
import qualified Miso.String                          as S
import           Models
import           Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import           Servant

newtype HtmlPage a = HtmlPage a
    deriving (Show, Eq)

type ServerAPI =
    ServerRoutes :<|> EntryAPI :<|> StaticAPI

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

app :: Application
app =
    serve (Proxy @ServerAPI)
        (    serverHandlers
        :<|> entryHandlers
        :<|> static
        )
    where
        static =
            serveDirectory "backend/static"

        serverHandlers = do
            serverPage homeLink :<|> serverPage activeLink :<|> serverPage completedLink

        entryHandlers =
            getEntries :<|> storeEntry :<|> updateEntry :<|> deleteEntry

serverPage :: URI -> ExceptT ServantErr IO (HtmlPage (View Msg))
serverPage link = do
    entries' <- getEntries
    return $ HtmlPage $ viewModel $ initialModel entries' link

storeEntry :: Entry -> ExceptT ServantErr IO ()
storeEntry Entry{..} =
    dbHandler $ do
        _ <- insert $ DbEntry description completed editing eid focussed
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
        _ <- deleteWhere [DbEntryEid ==. entryId]
        return ()

getEntries :: ExceptT ServantErr IO [Entry]
getEntries =
    dbHandler $ do
        entries' <- selectList [] []
        return $ Prelude.map (\(Entity _ e) -> entityToEntry e) entries'

mkApp :: IO Application
mkApp = do
    runDB (runMigration migrateAll)
    return app

run :: IO ()
run = do
    app' <- mkApp
    putStrLn "Running on port 3000"
    Wai.run 3000 $ Wai.logStdout app'

entityToEntry :: DbEntry -> Entry
entityToEntry DbEntry{..} =
    Entry {
        description = dbEntryDescription,
        completed = dbEntryCompleted,
        editing = dbEntryEditing,
        eid = dbEntryEid,
        focussed = dbEntryFocussed
    }
