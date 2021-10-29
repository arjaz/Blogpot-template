{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Main where

import           Blogpost
import           Data.Function                  ( (&) )
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( maybeToList )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.UUID                      ( UUID )
import qualified Database                      as Db
import qualified Database.PostgreSQL.Simple.Internal
                                               as Db
import qualified Opaleye                       as O
import           Polysemy                       ( Embed
                                                , Member
                                                , Members
                                                , Sem
                                                , embed
                                                , makeSem
                                                , reinterpret
                                                , runM
                                                )
import           Polysemy.Error                 ( Error )
import           Polysemy.Input                 ( Input )
import qualified Polysemy.Input                as Input
import           Polysemy.Output                ( Output )
import qualified Polysemy.Output               as Output
import           Servant
import           Servant.Polysemy.Server        ( runWarpServer )

data Database  k v m a where
  GetAll ::Database k v m [(k, v)]
  GetOne ::k -> Database k v m (Maybe v)
  AddOne ::v -> Database k v m k
  DeleteOne ::k -> Database k v m Bool

makeSem ''Database

type Logging = Output Text

type BlogpostsApi
  = "blogposts" :> QueryParam "id" UUID :> Get '[JSON] [(UUID, Blogpost)]
  :<|> "blogposts" :> ReqBody '[JSON] Blogpost :> Post '[JSON] UUID
  :<|> "blogposts" :> ReqBody '[JSON] UUID :> Delete '[JSON] Bool
type HealthcheckApi = "healthcheck" :> Get '[PlainText] Text

type Api = HealthcheckApi :<|> BlogpostsApi

main :: IO ()
main = do
  conn <- Db.dbConnection
  runWarpServer @Api 8081 True server
    -- Logging
    & Output.runOutputSem (embed . Text.putStrLn)
    -- Blogpost Database
    & runBlogpostDatabaseAsIO
    -- Database Connection
    & Input.runInputConst conn
    & runM

server
  :: Members '[Embed IO , Logging , Database UUID Blogpost] r
  => ServerT Api (Sem (Error ServerError : r))
server = healthcheck :<|> getBlogposts :<|> postBlogpost :<|> deleteBlogpost

healthcheck :: Member Logging r => Sem r Text
healthcheck = Output.output "healthcheck" $> "Healthcheck"

getBlogposts
  :: Members '[Database UUID Blogpost , Logging] r
  => Maybe UUID
  -> Sem r [(UUID, Blogpost)]
getBlogposts Nothing    = Output.output "Reading all blogposts" >> getAll
getBlogposts (Just bId) = do
  Output.output ("Reading blogpost with id " <> Text.pack (show bId))
  maybeToList . fmap (bId, ) <$> getOne bId

postBlogpost
  :: Members '[Database UUID Blogpost , Logging] r => Blogpost -> Sem r UUID
postBlogpost blogpost =
  Output.output ("Posting " <> Text.pack (show blogpost)) >> addOne blogpost

deleteBlogpost
  :: Members '[Database UUID Blogpost , Logging] r => UUID -> Sem r Bool
deleteBlogpost bId =
  Output.output ("Deletion of blogpost with id " <> Text.pack (show bId))
    >> deleteOne bId

runBlogpostDatabaseAsIO
  :: Member (Embed IO) r
  => Sem (Database UUID Blogpost : r) a
  -> Sem (Input Db.Connection : r) a
runBlogpostDatabaseAsIO = reinterpret $ \case
  GetAll -> do
    conn                              <- Input.input
    blogposts :: [(UUID, Text, Text)] <-
      embed . O.runSelect conn $ Db.blogpostsSelect
    pure . map (\(bId, name, body) -> (bId, Blogpost name body)) $ blogposts
  GetOne bId -> do
    conn                              <- Input.input
    blogposts :: [(UUID, Text, Text)] <-
      embed . O.runSelect conn $ Db.findBlogpostById bId
    case blogposts of
      ((_, name, body) : _) -> pure . Just $ Blogpost name body
      []                    -> pure Nothing
  AddOne (Blogpost name body) -> do
    conn     <- Input.input
    inserted <- embed . O.runInsert_ conn $ Db.insertBlogpost Nothing name body
    pure . head $ inserted
  DeleteOne bId -> do
    conn    <- Input.input
    deleted <- embed . O.runDelete_ conn $ Db.deleteBlogpost bId
    pure $ deleted /= 0

-- runKVStoreAsDatabase
--   :: Member (Embed IO) r
--   => Sem (KVStore UUID Blogpost : r) a
--   -> Sem (Input Db.Connection : r) a
-- runKVStoreAsDatabase = reinterpret $ \case
--   LookupKV bId -> do
--     conn                              <- input
--     blogposts :: [(UUID, Text, Text)] <- P.embed
--       $ O.runSelect conn (Db.findBlogpostById bId)
--     case blogposts of
--       ((_, name, body) : _) -> pure (Just (Blogpost name body))
--       []                    -> pure Nothing
--   UpdateKV bId Nothing -> do
--     conn <- input
--     _    <- P.embed $ O.runDelete_ conn (Db.deleteBlogpost bId)
--     pure ()
--   UpdateKV bId (Just (Blogpost newName newBody)) -> do
--     conn <- input
--     _    <- P.embed $ O.runUpdate_ conn (Db.updateBlogpost bId newName newBody)
--     pure ()


-- instance Logger AppM where
--   logInfo = liftIO . putStrLn . T.unpack

-- instance Db.Database ServerError UUID Blogpost AppM where
--   readAllFromDb = do
--     conn      <- asks appConfigDbConnection
--     blogposts <- liftIO $ O.runSelect conn Db.blogpostsSelect
--     pure $ map (\(_ :: UUID, name, body) -> Blogpost name body) blogposts
--   readOneFromDb id_ = do
--     conn  <- asks appConfigDbConnection
--     posts <- liftIO $ O.runSelect conn (Db.findBlogpostById id_)
--     case posts of
--       (_ :: UUID, name, body) : _ -> pure (Blogpost name body)
--       []                          -> throwError err404
--   writeToDb (Blogpost name body) = do
--     conn     <- asks appConfigDbConnection
--     inserted <- liftIO $ O.runInsert_ conn (Db.insertBlogpost Nothing name body)
--     case inserted of
--       id_ : _ -> pure id_
--       []      -> throwError err500
--   deleteFromDb id_ = do
--     conn         <- asks appConfigDbConnection
--     deletedCount <- liftIO $ O.runDelete_ conn (Db.deleteBlogpost id_)
--     pure (deletedCount /= 0)

-- healthcheck :: Logger m => m Text
-- healthcheck = do
--   logInfo "Healthcheck is hit"
--   pure "I'm alive"

-- getBlogposts :: (Logger m, Db.Database e k v m, Show k) => Maybe k -> m [v]
-- getBlogposts (Just bId) =
--   logInfo
--       ("Reading blogpost with id " <> T.pack (show bId) <> " from the database")
--     *>  Db.readOneFromDb bId
--     <&> (: [])
-- getBlogposts Nothing =
--   logInfo "Reading all blogposts from the database" *> Db.readAllFromDb

-- postBlogpost :: (Logger m, Db.Database e k v m, Show v) => v -> m k
-- postBlogpost v =
--   logInfo ("Adding " <> T.pack (show v) <> " to the database") *> Db.writeToDb v
