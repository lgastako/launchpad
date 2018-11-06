{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Launchpad.Prelude
  ( module X
  , definitely
  ) where

import Protolude   as X

import Control.Lens as X  (makeFields, makePrisms, (^.))
import Data.String as X ( String )

definitely :: Maybe a -> a
definitely = fromMaybe (panic "definitely not")
