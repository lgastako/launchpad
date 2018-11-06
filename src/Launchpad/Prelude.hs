{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Launchpad.Prelude
  ( module X
  , definitely
  ) where

import Control.Lens as X ( (^.)
                         , makeFields
                         , makePrisms
                         )
import Data.String  as X ( String )
import Protolude    as X

definitely :: Maybe a -> a
definitely = fromMaybe (panic "definitely not")
