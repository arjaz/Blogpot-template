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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Main where

import           Data.Function                  ( (&) )
import qualified Data.Text.IO                  as Text
import           Polysemy                       ( runM )
import           Servant.Polysemy.Server        ( runWarpServer )

import qualified Database
import qualified Logging
import qualified Server

main :: IO ()
main = do
  Text.putStrLn "Starting server on port 8081"
  runWarpServer @Server.Api 8081 True Server.server
    & Logging.runLoggingAsIO
    & Database.runBlogpostDatabaseAsIO
    & Database.runDatabaseConnectionAsIO
    & runM

