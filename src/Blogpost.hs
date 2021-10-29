{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Blogpost where

import           Data.Aeson              hiding ( json )
import           Data.Text                      ( Text )
import           GHC.Generics

data Blogpost = Blogpost
  { blogpostName :: Text
  , blogpostBody :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Blogpost
instance FromJSON Blogpost
