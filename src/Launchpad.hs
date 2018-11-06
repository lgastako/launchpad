{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Launchpad
  ( Launchpad
  , Board
  , Color
  , E8
  , Note(Note)
  , Point
  , Velocity(Velocity)
  , R(R)
  , G(G)
  , X(X)
  , Y(Y)
  , boardFromList
  , colorVelocity
  , connect
  , disconnect
  , emptyBoard
  , e8
  , notePoint
  , off
  , pointNote
  , sendBoard
  , sendRedGreen
  , snat
  ) where

-- TODO: constrain X/Y/Note to appropriate ranges

import Launchpad.Prelude   as P hiding ( state )

import Data.Type.Natural
import Data.Vector.Sized   as V
import System.MIDI
import System.MIDI.Utility

data Launchpad = Launchpad
  { _launchpadConnection :: Connection
  , _launchpadState      :: MVar LaunchpadState
  }

data LaunchpadState
  = Connected
  | Disconnected
  deriving (Eq, Ord, Read, Show)

type E8 = 'S ('S ('S ('S ('S ('S ('S ('S 'Z)))))))

e8 :: Sing E8
e8 = [snat| 8 |]

type Board = Vector Row E8
type Row   = Vector Color E8

newtype Note = Note Int
  deriving (Eq, Ord, Read, Show)

newtype Velocity = Velocity Int
  deriving (Eq, Ord, Read, Show)

type Color = (R, G)

newtype R = R Int
  deriving (Eq, Ord, Read, Show)

newtype G = G Int
  deriving (Eq, Ord, Read, Show)

type Point = (X, Y)

newtype X = X Int
  deriving (Eq, Ord, Read, Show)

newtype Y = Y Int
  deriving (Eq, Ord, Read, Show)

makeFields ''Launchpad
makePrisms ''LaunchpadState

-- TODO: replace with actually finding the launchpads and only showing a menu
--       if there is more than one
-- | Connect to a launchpad.
connect :: IO Launchpad
connect = do
  conn <- openDestination =<< selectOutputDevice prompt Nothing
  start conn
  var <- newMVar Connected
  return $ Launchpad conn var
  where
    prompt = "Select Launchpad device"

-- | Disconnect the launchpad.  The `Launchpad` value is not closed and can no
-- longer be used.
disconnect :: Launchpad -> IO ()
disconnect lp = readMVar st >>= \case
  Connected    -> close conn >> void (modifyMVar_ st dis)
  Disconnected -> mempty
  where
    conn = lp ^. connection
    st   = lp ^. state
    dis  = const $ pure Disconnected

-- | Convert a column-row vector into the appropriate note to send to the Launchpad.
pointNote :: Point -> Note
pointNote (X x, Y y) = Note $ (0x10 * y) + x

-- | Convert a note from the Launchpad into a column-row vector.
notePoint :: Note -> Point
notePoint (Note n) = (X x, Y y)
  where
    x = complement $ n .&. 0xfff0
    y = shiftR n 0x04

-- | Convert a Color to the appropriate velocity for the Launchpad.
colorVelocity :: Color -> Velocity
colorVelocity (R r, G g) = Velocity $ 0x10 * g + r + flags
  where
    flags = 0  -- for now
  -- Bit 6 must be 0
  -- 5..4 are the green values
  -- 3 clear (ignored for now)
  -- 2 copy (ignored for now)
  -- 1..0 red

boardFromList :: [[Color]] -> Maybe Board
boardFromList xs = join (fromList e8 <$> traverse (fromList e8) xs)

boardToList :: Board -> [(Point, Color)]
boardToList board =
  [ ((X colNum, Y rowNum), color)
  | (rowNum, row) <- listOfLists
  , (colNum, color) <- row
  ]
  where
    listOfRows :: [(Int, Row)]
    listOfRows = enumerate $ V.toList board

    listOfLists :: [(Int, [(Int, Color)])]
    listOfLists = fmap f listOfRows

    f :: (Int, Row) -> (Int, [(Int, Color)])
    f (rowNum, row) = (rowNum, enumerate $ V.toList row)

enumerate :: [a] -> [(Int, a)]
enumerate = P.zip [0..]

-- | Send the given board to the launchpad.
sendBoard :: Launchpad -> Board -> IO ()
sendBoard lp board = traverse_ (uncurry (sendRedGreen lp)) colors
  where
    colors :: [(Point, Color)]
    colors = boardToList board

-- | Render the given red/green values to the row/col of receiver.
sendRedGreen :: Launchpad -> (X, Y) -> (R, G) -> IO ()
sendRedGreen lp point color = midiNoteOn (lp ^. connection) note' velocity
  where
    note'    = pointNote point
    velocity = colorVelocity color

midiNoteOn :: Connection -> Note -> Velocity -> IO ()
midiNoteOn conn (Note n) (Velocity v) = do
  putStrLn $ "midiNoteOn: " <> show (Note n, Velocity v)
  send conn $ MidiMessage chan $ NoteOn n v
  where
    chan = 1  -- Launchpad is (nearly) always on channel one.

off :: Launchpad -> IO ()
off = flip sendBoard emptyBoard

emptyBoard :: Board
emptyBoard = V.replicate e8 row
  where
    row = V.replicate e8 (R 0, G 0)
