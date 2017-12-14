{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Foundation where

import           Data.Aeson   hiding (Object)
import           Data.Bool
import qualified Data.Map     as M
import           Data.Monoid
import           GHC.Generics
import           Miso
import           Miso.String  (MisoString)
import qualified Miso.String  as S

data Model = Model
  { entries :: [Entry]
  , field :: MisoString
  , uid :: Int
  , visibility :: MisoString
  , step :: Bool
  } deriving (Show, Generic, Eq)

data Entry = Entry
  { description :: MisoString
  , completed :: Bool
  , editing :: Bool
  , eid :: Int
  , focussed :: Bool
  } deriving (Show, Generic, Eq)

instance ToJSON Entry
instance ToJSON Model

instance FromJSON Entry
instance FromJSON Model
