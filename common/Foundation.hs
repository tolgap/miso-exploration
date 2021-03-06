{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Foundation where

import           Data.Aeson       hiding (Object)
import           Data.Bool
import qualified Data.Map         as M
import           Data.Monoid
import           Data.Proxy
import           GHC.Generics
import           Miso
import           Miso.String      (MisoString)
import qualified Miso.String      as S
import           Network.URI      (pathSegments)
import           Servant.API
import           System.IO.Unsafe (unsafePerformIO)

data Msg
  = NoOp
  | HandleURIChange URI
  | ChangeURI URI
  | UpdateField MisoString
  | EditingEntry Bool Int
  | UpdateEntry MisoString Int
  | Add
  | Delete Int
  | DeleteComplete
  | Check Bool Int
  | CheckAll Bool
  | FetchEntries
  | EntriesResult [Entry]
   deriving Show

data Model = Model
  { _entries    :: [Entry]
  , _currentURI :: URI
  , _field      :: MisoString
  , _uid        :: Int
  , _step       :: Bool
  , _uri        :: URI
  } deriving (Show, Generic, Eq)

data Entry = Entry
    { description :: MisoString
    , completed   :: Bool
    , editing     :: Bool
    , eid         :: Int
    , focussed    :: Bool
} deriving (Show, Generic, Eq)

instance ToJSON Entry
instance FromJSON Entry

type ClientRoutes = Home :<|> Active :<|> Completed
type Home = View Msg
type Active = "active" :> View Msg
type Completed = "completed" :> View Msg

debugLog :: (Show a) => a -> a
debugLog x =
    let !_ = unsafePerformIO (print x) in x

initialModel :: [Entry] -> URI -> Model
initialModel initialEntries currentURI' = Model
  { _entries = initialEntries
  , _currentURI = currentURI'
  , _field = mempty
  , _uid = (+ 1) $ safeMaximum $ map eid initialEntries
  , _step = False
  , _uri = homeLink
  }

safeMaximum :: (Ord t, Num t) => [t] -> t
safeMaximum [] = 0
safeMaximum xs = maximum xs

homeLink :: URI
homeLink =
    safeLink (Proxy @ClientRoutes) (Proxy @Home)

activeLink :: URI
activeLink =
    safeLink (Proxy @ClientRoutes) (Proxy @Active)

completedLink :: URI
completedLink =
    safeLink (Proxy @ClientRoutes) (Proxy @Completed)

viewModel :: Model -> View Msg
viewModel model =
    case runRoute (Proxy @ClientRoutes) viewTree _currentURI model of
        Left _routingError -> page404View
        Right r            -> r

page404View :: View Msg
page404View =
    text "Yo, 404, page unknown. Go to /. Shoo!"

viewTree
    ::       (Model -> View Msg)
        :<|> (Model -> View Msg)
        :<|> (Model -> View Msg)
viewTree = viewPage :<|> viewPage :<|> viewPage

viewPage :: Model -> View Msg
viewPage m@Model{..} =
 div_
    [ class_ "todomvc-wrapper"
    , style_  $ M.singleton "visibility" "hidden"
    ]
    [ section_
        [ class_ "todoapp" ]
        [ viewInput m _field
        , viewEntries _currentURI _entries
        , viewControls _currentURI _entries
        ]
    , infoFooter
    ]

viewEntries :: URI -> [ Entry ] -> View Msg
viewEntries currentURI' entries =
  section_
    [ class_ "main"
    , style_ $ M.singleton "visibility" cssVisibility
    ]
    [ input_
        [ class_ "toggle-all"
        , type_ "checkbox"
        , name_ "toggle"
        , checked_ allCompleted
        , onClick $ CheckAll (not allCompleted)
        ]
      , label_
        [ for_ "toggle-all" ]
          [ text $ S.pack "Mark all as complete" ]
      , ul_ [ class_ "todo-list" ] $
         flip map (filter isVisible entries) $ \t ->
           viewKeyedEntry t
      ]
  where
    cssVisibility = bool "visible" "hidden" (null entries)
    allCompleted = all (==True) $ completed <$> entries
    isVisible Entry {..} =
      if pathSegments currentURI' == pathSegments activeLink
        then not completed
      else if pathSegments currentURI' == pathSegments completedLink
        then completed
      else True

viewKeyedEntry :: Entry -> View Msg
viewKeyedEntry = viewEntry

viewEntry :: Entry -> View Msg
viewEntry Entry {..} = liKeyed_ (toKey eid)
    [ class_ $ S.intercalate " " $
       [ "completed" | completed ] <> [ "editing" | editing ]
    ]
    [ div_
        [ class_ "view" ]
        [ input_
            [ type_ "checkbox"
            , class_ "toggle"
            , checked_ completed
            , onClick $ Check (not completed) eid
            ]
        , label_
            [ onDoubleClick $ EditingEntry True eid ]
            [ text description ]
        , button_
            [ class_ "destroy"
            , onClick $ Delete eid
            ] []
        ]
    , input_
        [ value_ description
        , name_ "title"
        , id_ $ "todo-" <> S.pack (show eid)
        , class_ "edit"
        , onInput $ flip UpdateEntry eid
        , onBlur $ EditingEntry False eid
        , onEnter $ EditingEntry False eid
        ]
    ]

viewControls :: URI -> [ Entry ] -> View Msg
viewControls uri entries =
  footer_  [ class_ "footer"
           , hidden_ (null entries)
           ]
      [ viewControlsCount entriesLeft
      , viewControlsFilters uri
      , viewControlsClear entriesCompleted
      ]
  where
    entriesCompleted = length . filter completed $ entries
    entriesLeft = length entries - entriesCompleted

viewControlsCount :: Int -> View Msg
viewControlsCount entriesLeft =
  span_ [ class_ "todo-count" ]
     [ strong_ [] [ text $ S.pack (show entriesLeft) ]
     , text (item_ <> " left")
     ]
  where
    item_ = S.pack $ bool " items" " item" (entriesLeft == 1)

viewControlsFilters :: URI -> View Msg
viewControlsFilters currentURI =
  ul_
    [ class_ "filters" ]
    [ visibilitySwap "All" homeLink currentURI
    , text " "
    , visibilitySwap "Active" activeLink currentURI
    , text " "
    , visibilitySwap "Completed" completedLink currentURI
    ]

visibilitySwap :: MisoString -> URI -> URI -> View Msg
visibilitySwap visibility' uri currentURI' =
  li_ [  ]
      [ a_ [ href_ (S.pack href)
           , class_ $ S.concat [ "selected" | uri == currentURI' ]
           , onClick (ChangeURI uri)
           ] [ text visibility' ]
      ]
  where
    href = "/" ++ show uri

viewControlsClear :: Int -> View Msg
viewControlsClear entriesCompleted =
  button_
    [ class_ "clear-completed"
    , prop "hidden" (entriesCompleted == 0)
    , onClick DeleteComplete
    ]
    [ text $ "Clear completed (" <> S.pack (show entriesCompleted) <> ")" ]

viewInput :: Model -> MisoString -> View Msg
viewInput _ task =
  header_ [ class_ "header" ]
    [ h1_ [] [ text "todos" ]
    , input_
        [ class_ "new-todo"
        , placeholder_ "What needs to be done?"
        , autofocus_ True
        , value_ task
        , name_ "newTodo"
        , onInput UpdateField
        , onEnter Add
        ]
    ]

onEnter :: Msg -> Attribute Msg
onEnter action =
  onKeyDown $ bool NoOp action . (== KeyCode 13)

infoFooter :: View Msg
infoFooter =
    footer_ [ class_ "info" ]
    [ p_ [] [ text "Double-click to edit a todo" ]
    , p_ []
        [ text "Written by "
        , a_ [ href_ "https://github.com/dmjio" ] [ text "David Johnson" ]
        ]
    , p_ []
        [ text "Part of "
        , a_ [ href_ "http://todomvc.com" ] [ text "TodoMVC" ]
        ]
    ]
