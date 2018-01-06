{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import           Data.Text
import           Servant.API

import           Foundation  (Entry)

type EntryAPI = "entries" :> Get '[JSON] [Entry]
                :<|> "entries" :> ReqBody '[JSON] Entry :> Post '[JSON] ()
                :<|> "entries" :> Capture "id" Int :> ReqBody '[JSON] Entry :> Put '[JSON] ()
                :<|> "entries" :> Capture "id" Int :> Delete '[JSON] ()
type StaticAPI = "static" :> Raw
