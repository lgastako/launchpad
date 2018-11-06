{-# LANGUAGE NoImplicitPrelude #-}

module Launchpad.Demos where

import Launchpad.Prelude

import Launchpad
import Launchpad.Colors

xmasTreeBoard :: Board
xmasTreeBoard = definitely . boardFromList $
  [ [r0g0, r0g0, r0g0, r0g3, r0g3, r0g0, r0g0, r0g0]
  , [r0g0, r0g0, r0g2, r0g3, r0g3, r0g2, r0g0, r0g0]
  , [r0g0, r0g0, r0g3, r0g3, r0g3, r0g3, r0g0, r0g0]
  , [r0g0, r0g2, r0g3, r0g3, r0g3, r0g3, r0g2, r0g0]
  , [r0g0, r0g3, r0g3, r0g3, r0g3, r0g3, r0g3, r0g0]
  , [r0g2, r0g3, r0g3, r0g3, r0g3, r0g3, r0g3, r0g2]
  , [r0g3, r0g3, r0g3, r0g3, r0g3, r0g3, r0g3, r0g3]
  , [r0g0, r0g0, r0g0, r3g3, r3g3, r0g0, r0g0, r0g0]
  ]

smileyBoard :: Board
smileyBoard = definitely . boardFromList $
  [ [e, e, y, y, y, y, e, e]
  , [e, y, y, y, y, y, y, e]
  , [y, y, e, y, y, e, y, y]
  , [y, y, y, y, y, y, y, y]
  , [y, e, y, y, y, y, e, y]
  , [y, y, e, y, y, e, y, y]
  , [e, y, y, e, e, y, y, e]
  , [e, e, y, y, y, y, e, e]
  ]

demoBoard :: Maybe Launchpad -> Board -> IO ()
demoBoard lpMay board = flip sendBoard board =<< case lpMay of
  Just lp -> pure lp
  Nothing -> connect

demoBoard_ :: Board -> IO ()
demoBoard_ = demoBoard Nothing

demoXmasTree :: IO ()
demoXmasTree = demoBoard_ xmasTreeBoard

demoSmiley :: IO ()
demoSmiley = demoBoard_ smileyBoard
