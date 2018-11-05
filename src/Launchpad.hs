{-# LANGUAGE NoImplicitPrelude #-}

module Launchpad where

import Launchpad.Prelude

data Spot = Empty | MoreSoon
  deriving (Eq, Ord, Read, Show)
