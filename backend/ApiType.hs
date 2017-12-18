{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import           Data.Text
import           Servant.API

import           Foundation  (Entry)

type EntryAPI = "entries" :> Get '[JSON] [Entry]
type StaticAPI = "static" :> Raw
