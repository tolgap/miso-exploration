{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main(main) where

import           Data.Aeson                    hiding (Object)
import           Data.Bool
import           Data.ByteString.Lazy.Char8    (unpack)
import qualified Data.JSString                 as JSS
import qualified Data.Map                      as M
import           Data.Monoid
import           Foundation
import           GHC.Generics
import           JavaScript.Web.XMLHttpRequest
import           Miso
import           Miso.String                   (MisoString)
import qualified Miso.String                   as S

instance HasURI Model where
    lensURI = uri

newEntry :: MisoString -> Int -> Entry
newEntry desc eid = Entry
  { description = desc
  , completed = False
  , editing = False
  , eid = eid
  , focussed = False
  }

main :: IO ()
main = do
    initialEntries <- getEntries
    miso App
        { initialAction = NoOp
        , model = initialModel initialEntries
        , update = updateModel
        , view = viewModel
        , events = defaultEvents
        , subs = []
        , mountPoint = Nothing
        }

updateModel :: Msg -> Model -> Effect Msg Model
updateModel NoOp m = noEff m
updateModel (CurrentTime n) m =
  m <# (print n >> pure NoOp)
updateModel Add model@Model{..} =
    model
        {
        _uid = _uid + 1
        , _field = mempty
        , _entries = _entries <> [ newEntry _field _uid ]
        } <# do
            NoOp <$ postEntry (newEntry _field _uid)

updateModel (UpdateField str) model = noEff model { _field = str }
updateModel (EditingEntry id' isEditing) model@Model{..} =
  model { _entries = newEntries } <# do
    focus $ S.pack $ "todo-" ++ show id'
    pure NoOp
    where
      newEntries = filterMap _entries (\t -> eid t == id') $
         \t -> t { editing = isEditing, focussed = isEditing }

updateModel (UpdateEntry id' task) model@Model{..} =
  noEff model { _entries = newEntries }
    where
      newEntries =
        filterMap _entries ((==id') . eid) $ \t ->
           t { description = task }

updateModel (Delete id') model@Model{..} =
  noEff model { _entries = filter (\t -> eid t /= id') _entries }

updateModel DeleteComplete model@Model{..} =
  noEff model { _entries = filter (not . completed) _entries }

updateModel (Check id' isCompleted) model@Model{..} =
   model { _entries = newEntries } <# eff
    where
      eff =
        putStrLn "clicked check" >>
          pure NoOp

      newEntries =
        filterMap _entries (\t -> eid t == id') $ \t ->
          t { completed = isCompleted }

updateModel (CheckAll isCompleted) model@Model{..} =
  noEff model { _entries = newEntries }
    where
      newEntries =
        filterMap _entries (const True) $
          \t -> t { completed = isCompleted }

updateModel (ChangeVisibility v) model =
  noEff model { _visibility = v }

updateModel FetchEntries model =
    model <# do
        EntriesResult <$> getEntries

updateModel (EntriesResult newEntries) model =
    noEff $ initialModel newEntries

filterMap :: [a] -> (a -> Bool) -> (a -> a) -> [a]
filterMap xs predicate f = go' xs
  where
    go' [] = []
    go' (y:ys)
     | predicate y = f y : go' ys
     | otherwise   = y : go' ys

getEntries :: IO [Entry]
getEntries = do
    Just resp <- contents <$> xhrByteString req
    case eitherDecodeStrict resp :: Either String [Entry] of
        Left s  -> error s
        Right j -> pure j
    where
        req = Request { reqMethod = GET
                      , reqURI = S.pack "/entries"
                      , reqLogin = Nothing
                      , reqHeaders = []
                      , reqWithCredentials = False
                      , reqData = NoData
                      }

postEntry :: Entry -> IO ()
postEntry entry = do
    Just resp <- contents <$> xhrByteString req
    case eitherDecodeStrict resp :: Either String () of
        Left s  -> error s
        Right _ -> pure ()
    where
        req = Request { reqMethod = POST
                      , reqURI = S.pack "/entries"
                      , reqLogin = Nothing
                      , reqHeaders = [ (JSS.pack "Content-Type", JSS.pack "application/json") ]
                      , reqWithCredentials = False
                      , reqData = StringData $ JSS.pack $ unpack $ encode entry
                      }
