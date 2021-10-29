{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}


module Logging where

import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as Text
import           Polysemy                       ( Embed
                                                , Member
                                                , Sem
                                                , embed
                                                )
import           Polysemy.Output                ( Output )
import qualified Polysemy.Output               as Output

type Logging = Output Text

runLoggingAsIO :: Member (Embed IO) r => Sem (Logging : r) a -> Sem r a
runLoggingAsIO = Output.runOutputSem (embed . Text.putStrLn)
