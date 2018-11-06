{-# LANGUAGE NoImplicitPrelude #-}

module Launchpad.Colors where

import Launchpad.Prelude hiding ( max
                                , min
                                )

import Launchpad

off :: Int
off = 0

min :: Int
min = 1

mid :: Int
mid = 2

max :: Int
max = 3

e :: Color
e = (R off, G off)

r :: Color
r = (R max, G off)

g :: Color
g = (R off, G max)

y :: Color
y = (R max, G max)

r0g0 :: Color
r0g0 = e

r0g1 :: Color
r0g1 = (R off, G min)

r0g2 :: Color
r0g2 = (R off, G mid)

r0g3 :: Color
r0g3 = (R off, G max)

r1g0 :: Color
r1g0 = (R min, G off)

r1g1 :: Color
r1g1 = (R min, G min)

r1g2 :: Color
r1g2 = (R min, G mid)

r1g3 :: Color
r1g3 = (R min, G max)

r2g0 :: Color
r2g0 = (R mid, G off)

r2g1 :: Color
r2g1 = (R mid, G min)

r2g2 :: Color
r2g2 = (R mid, G mid)

r2g3 :: Color
r2g3 = (R mid, G max)

r3g0 :: Color
r3g0 = (R max, G off)

r3g1 :: Color
r3g1 = (R max, G min)

r3g2 :: Color
r3g2 = (R max, G mid)

r3g3 :: Color
r3g3 = (R max, G max)
