module AppConfig where

import           Data.Text                      ( Text )

data AppConfig = AppConfig
  { appConfigUser :: Text
  , appConfigPass :: Text
  }
