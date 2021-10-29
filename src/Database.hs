{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Database where

import           Data.Profunctor.Product        ( p3 )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple     ( connectPostgreSQL )
import qualified Database.PostgreSQL.Simple.Internal
                                               as Db
import           GHC.Int                        ( Int64 )
import           Opaleye

type WriteBlogpostField = (Maybe (Field SqlUuid), Field SqlText, Field SqlText)
type ReadBlogpostField = (Field SqlUuid, Field SqlText, Field SqlText)

blogpostsTable :: Table WriteBlogpostField ReadBlogpostField
blogpostsTable =
  table "blogposts" (p3 (tableField "id", tableField "name", tableField "body"))

blogpostsSelect :: Select ReadBlogpostField
blogpostsSelect = selectTable blogpostsTable

findBlogpostById :: UUID -> Select ReadBlogpostField
findBlogpostById findId = do
  row@(id_, _, _) <- blogpostsSelect
  where_ $ id_ .== toFields findId
  pure row

insertBlogpost :: Maybe UUID -> Text -> Text -> Insert [UUID]
insertBlogpost id_ name body = Insert
  { iTable      = blogpostsTable
  , iRows       = [(toFields id_, toFields name, toFields body)]
  , iReturning  = rReturning (\(rId, _, _) -> rId)
  , iOnConflict = Nothing
  }

updateBlogpost :: UUID -> Text -> Text -> Update Int64
updateBlogpost id_ name body = Update
  { uTable      = blogpostsTable
  , uUpdateWith = updateEasy
                    (\(bId, _, _) -> (bId, toFields name, toFields body))
  , uWhere      = \(bId, _, _) -> bId .== toFields id_
  , uReturning  = rCount
  }

deleteBlogpost :: UUID -> Delete Int64
deleteBlogpost id_ = Delete { dTable = blogpostsTable
                            , dWhere = \(dbId, _, _) -> dbId .== toFields id_
                            , dReturning = rCount
                            }

dbConnection :: IO Db.Connection
dbConnection = connectPostgreSQL "dbname='blogpot'"
