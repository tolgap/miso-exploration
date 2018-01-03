{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import           Data.Text
import           Servant.API

import           Foundation  (Entry)

type EntryAPI = "entries" :> Get '[JSON] [Entry]
                :<|> "entries" :> ReqBody '[JSON] Entry :> Post '[JSON] ()
type StaticAPI = "static" :> Raw
