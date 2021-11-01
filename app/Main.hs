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

module Main where

import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Network.Wai.Handler.Warp      as Warp
import qualified System.Environment            as Env

import           AppConfig
import qualified Server

main :: IO ()
main = do
  Text.putStrLn "Starting server on port 8081"
  user <- maybe "servant" Text.pack <$> Env.lookupEnv "BLOGPOT_USER"
  pass <- maybe "servant" Text.pack <$> Env.lookupEnv "BLOGPOT_PASS"
  let config = AppConfig { appConfigUser = user, appConfigPass = pass }
  Warp.run 8081 (Server.serveSemServer Server.server config)
