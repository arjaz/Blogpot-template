{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Server where

import           Blogpost
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( maybeToList )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.UUID                      ( UUID )
import           Database                       ( Database )
import qualified Database
import           Logging                        ( Logging )
import qualified Logging
import           Polysemy                       ( Embed
                                                , Member
                                                , Members
                                                , Sem
                                                , runM
                                                )
import           Polysemy.Error                 ( Error )
import qualified Polysemy.Output               as Output
import           Servant
import           Servant.Polysemy.Server

type BlogpostsApi
  = "blogposts" :> QueryParam "id" UUID :> Get '[JSON] [(UUID, Blogpost)]
  :<|> BasicAuth "post-realm" () :> "blogposts" :> ReqBody '[JSON] Blogpost :> Post '[JSON] UUID
  :<|> BasicAuth "post-realm" () :> "blogposts" :> ReqBody '[JSON] UUID :> Delete '[JSON] Bool
type HealthcheckApi = "healthcheck" :> Get '[PlainText] Text

type Api = HealthcheckApi :<|> BlogpostsApi

api :: Proxy Api
api = Proxy

authCheck :: BasicAuthCheck ()
authCheck =
  -- TODO: make it configurable
  let check (BasicAuthData username password) =
        if username == "servant" && password == "servant"
          then pure (Authorized ())
          else pure Unauthorized
  in  BasicAuthCheck check

basicAuthServerContext :: Context (BasicAuthCheck () : '[])
basicAuthServerContext = authCheck :. EmptyContext

basicAuthServerContextProxy :: Proxy '[BasicAuthCheck ()]
basicAuthServerContextProxy = Proxy

serveSemServer
  :: ServerT
       Api
       (Sem '[Error ServerError , Logging , Database UUID Blogpost , Embed IO])
  -> Application
serveSemServer s = serveWithContext
  api
  basicAuthServerContext
  (hoistServerWithContext api
                          basicAuthServerContextProxy
                          (semHandler lowerToIO)
                          s
  )
 where
  lowerToIO =
    runM
      . Database.runDatabaseConnectionAsIO
      . Database.runBlogpostDatabaseAsIO
      . Logging.runLoggingAsIO

server
  :: Members '[Logging , Database UUID Blogpost] r
  => ServerT Api (Sem (Error ServerError : r))
server =
  healthcheck
    :<|> getBlogposts
    -- const as we don't care about the data returned from the basic auth check
    :<|> const postBlogpost
    :<|> const deleteBlogpost

healthcheck :: Member Logging r => Sem r Text
healthcheck = Output.output "healthcheck" $> "Healthcheck"

getBlogposts
  :: Members '[Database UUID Blogpost , Logging] r
  => Maybe UUID
  -> Sem r [(UUID, Blogpost)]
getBlogposts Nothing = Output.output "Reading all blogposts" >> Database.getAll
getBlogposts (Just bId) = do
  Output.output ("Reading blogpost with id " <> Text.pack (show bId))
  maybeToList . fmap (bId, ) <$> Database.getOne bId

postBlogpost
  :: Members '[Database UUID Blogpost , Logging] r => Blogpost -> Sem r UUID
postBlogpost blogpost = Output.output ("Posting " <> Text.pack (show blogpost))
  >> Database.addOne blogpost

deleteBlogpost
  :: Members '[Database UUID Blogpost , Logging] r => UUID -> Sem r Bool
deleteBlogpost bId =
  Output.output ("Deletion of blogpost with id " <> Text.pack (show bId))
    >> Database.deleteOne bId
