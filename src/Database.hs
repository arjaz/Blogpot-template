{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
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

import           Blogpost
import           Data.Profunctor.Product        ( p3 )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple     ( connectPostgreSQL )
import           Database.PostgreSQL.Simple.Internal
                                                ( Connection )
import           GHC.Int                        ( Int64 )
import           Opaleye
import           Polysemy                       ( Embed
                                                , Member
                                                , Sem
                                                , embed
                                                , makeSem
                                                , reinterpret
                                                )
import           Polysemy.Input                 ( Input )
import qualified Polysemy.Input                as Input

data Database k v m a where
  GetAll ::Database k v m [(k, v)]
  GetOne ::k -> Database k v m (Maybe v)
  AddOne ::v -> Database k v m k
  DeleteOne ::k -> Database k v m Bool

makeSem ''Database

runBlogpostDatabaseAsIO
  :: Member (Embed IO) r
  => Sem (Database UUID Blogpost : r) a
  -> Sem (Input Connection : r) a
runBlogpostDatabaseAsIO = reinterpret $ \case
  GetAll -> do
    conn                              <- Input.input
    blogposts :: [(UUID, Text, Text)] <-
      embed . runSelect conn $ blogpostsSelect
    pure . map (\(bId, name, body) -> (bId, Blogpost name body)) $ blogposts
  GetOne bId -> do
    conn                              <- Input.input
    blogposts :: [(UUID, Text, Text)] <-
      embed . runSelect conn $ findBlogpostById bId
    case blogposts of
      ((_, name, body) : _) -> pure . Just $ Blogpost name body
      []                    -> pure Nothing
  AddOne (Blogpost name body) -> do
    conn     <- Input.input
    inserted <- embed . runInsert_ conn $ insertBlogpost Nothing name body
    pure . head $ inserted
  DeleteOne bId -> do
    conn    <- Input.input
    deleted <- embed . runDelete_ conn $ deleteBlogpost bId
    pure $ deleted /= 0

runDatabaseConnectionAsIO
  :: Member (Embed IO) r => Sem (Input Connection : r) a -> Sem r a
runDatabaseConnectionAsIO r = do
  conn <- embed connection
  Input.runInputConst conn r

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

connection :: IO Connection
connection = connectPostgreSQL "dbname='blogpot'"

