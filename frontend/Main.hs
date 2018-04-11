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
import           Data.List                     (find)
import qualified Data.Map                      as M
import           Data.Monoid
import           Foundation
import           GHC.Generics
import           JavaScript.Web.XMLHttpRequest
import           Miso
import           Miso.String                   (MisoString)
import qualified Miso.String                   as S

batchEff :: model -> [IO action] -> Effect action model
batchEff m actions = Effect m (map (\a -> \sink -> a >>= sink) actions)

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
    entries <- getEntries
    miso $ \currentURI ->
        App
            { initialAction = NoOp
            , model = initialModel entries currentURI
            , update = updateModel
            , view = viewModel
            , events = defaultEvents
            , subs = [ Miso.uriSub HandleURIChange ]
            , mountPoint = Nothing
            }

updateModel :: Msg -> Model -> Effect Msg Model
updateModel NoOp m = noEff m
updateModel (ChangeURI uri) model =
    fromTransition (scheduleIO $ do
        pushURI uri
        pure NoOp) model
updateModel (HandleURIChange uri) model =
    model
        { _currentURI = uri }
        <# do
            pure NoOp
updateModel Add model@Model{..} =
    model
        {
        _uid = _uid + 1
        , _field = mempty
        , _entries = _entries <> [ newEntry _field _uid ]
        } <# do
            NoOp <$ postEntry (newEntry _field _uid)
updateModel (UpdateField str) model = noEff model { _field = str }
updateModel (EditingEntry isEditing id') model@Model{..} =
  model { _entries = newEntries } <# do
    focus $ S.pack $ "todo-" ++ show id'
    pure NoOp
    where
      newEntries = filterMap _entries (\t -> eid t == id') $
         \t -> t { editing = isEditing, focussed = isEditing }

updateModel (UpdateEntry task id') model@Model{..} =
  noEff model { _entries = newEntries }
    where
      newEntries =
        filterMap _entries ((==id') . eid) $ \t ->
           t { description = task }

updateModel (Delete id') model@Model{..} =
  model { _entries = filter (\t -> eid t /= id') _entries } <# do
      NoOp <$ deleteEntry id'

updateModel DeleteComplete model@Model{..} =
  batchEff model $ map (pure . Delete . eid) (filter completed _entries)

updateModel (Check isCompleted id') model@Model{..} =
   model { _entries = newEntries } <# eff
    where
      eff =
        maybe (return ()) patchEntry (findEntry id' newEntries)
            >> pure NoOp
      newEntries =
        filterMap _entries (\t -> eid t == id') $ \t ->
          t { completed = isCompleted }

updateModel (CheckAll isCompleted) model@Model{..} =
  batchEff model $ map (pure . Check isCompleted . eid) _entries

updateModel FetchEntries model =
    model <# do
        EntriesResult <$> getEntries

updateModel (EntriesResult newEntries) model@Model{..} =
    noEff $ initialModel newEntries _currentURI

filterMap :: [a] -> (a -> Bool) -> (a -> a) -> [a]
filterMap xs predicate f = go' xs
  where
    go' [] = []
    go' (y:ys)
     | predicate y = f y : go' ys
     | otherwise   = y : go' ys

findEntry :: Int -> [Entry] -> Maybe Entry
findEntry eid' =
    find (\e -> eid e == eid')

deleteEntry :: Int -> IO ()
deleteEntry id' =
    xhrRequest url DELETE NoData
    where
        url = "/entries/" ++ show id'

patchEntry :: Entry -> IO ()
patchEntry entry =
    xhrRequest url PUT data'
    where
        url = "/entries/" ++ show (eid entry)
        data' = StringData $ JSS.pack $ unpack $ encode entry

getEntries :: IO [Entry]
getEntries =
    xhrRequest "/entries" GET NoData

postEntry :: Entry -> IO ()
postEntry entry =
    xhrRequest "/entries" POST data'
    where
        data' = StringData $ JSS.pack $ unpack $ encode entry

xhrRequest url method' reqData' = do
    Just resp <- contents <$> xhrByteString req
    either error pure (eitherDecodeStrict resp)
    where
        req = Request { reqMethod = method'
                      , reqURI = S.pack url
                      , reqLogin = Nothing
                      , reqHeaders = [ (JSS.pack "Content-Type", JSS.pack "application/json") ]
                      , reqWithCredentials = False
                      , reqData = reqData'
                      }
