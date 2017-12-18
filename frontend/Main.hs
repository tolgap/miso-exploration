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

import           Data.Aeson   hiding (Object)
import           Data.Bool
import qualified Data.Map     as M
import           Data.Monoid
import           Foundation
import           GHC.Generics
import           Miso
import           Miso.String  (MisoString)
import qualified Miso.String  as S

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
main = miso App
    { initialAction = NoOp
    , model = initialModel
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
  noEff model {
    _uid = _uid + 1
  , _field = mempty
  , _entries = _entries <> [ newEntry _field _uid | not $ S.null _field ]
  }
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

filterMap :: [a] -> (a -> Bool) -> (a -> a) -> [a]
filterMap xs predicate f = go' xs
  where
    go' [] = []
    go' (y:ys)
     | predicate y = f y : go' ys
     | otherwise   = y : go' ys
