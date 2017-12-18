{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           ApiType                  (EntryAPI, StaticAPI)
import           Control.Monad.IO.Class
import           Control.Monad.Logger     (runStderrLoggingT)
import           Data.Proxy
import           Data.Text                as T
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           Foundation
import qualified Lucid                    as L
import qualified Lucid.Base               as L
import           Miso                     (ToServerRoutes)
import qualified Miso.String              as S
import           Models
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp
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

app pool =
    serve (Proxy @ServerAPI) (serverRouteHandlers :<|> static :<|> entryHandlers)
    where
        static =
            serveDirectory "backend/static"

        serverRouteHandlers = do
            entries <- liftIO $ getEntries pool
            pure $ HtmlPage $ viewModel (initialModel entries)

        entryHandlers =
            liftIO $ getEntries pool

getEntries pool =
    flip runSqlPersistMPool pool $ do
        entries <- selectList [] []
        return $ Prelude.map (\(Entity _ e) -> entityToEntry e) entries

mkApp = do
    pool <- runStderrLoggingT $ createSqlitePool "db/miso-test.sqlite3" 1
    runSqlPool (runMigration migrateAll) pool
    return $ app pool

run :: IO ()
run =
    Warp.run 3000 =<< mkApp

entityToEntry DbEntry{..} =
    Entry {
        description = S.pack $ T.unpack dbEntryDescription,
        completed = dbEntryCompleted,
        editing = dbEntryEditing,
        eid = dbEntryEid,
        focussed = dbEntryFocussed
    }
