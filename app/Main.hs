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

import qualified Data.Text.IO                  as Text
import qualified Network.Wai.Handler.Warp      as Warp

import qualified Server

main :: IO ()
main = do
  Text.putStrLn "Starting server on port 8081"
  Warp.run 8081 (Server.serveSemServer Server.server)
