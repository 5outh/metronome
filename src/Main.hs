{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Options.Generic
import System.Directory
import Control.Concurrent
import Control.Monad
import Data.List
import Turtle hiding (time)
import Data.Text (pack)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe

type SoundMap = M.Map Text Text

type Subdivision = Int

getSubdivisionWords :: Subdivision -> [Text]
getSubdivisionWords = T.words . \case
  1 -> ""
  2 -> "n"
  3 -> "uh let"
  4 -> "e n uh"
  5 -> "ka ta ka ta"
  6 -> "ta la ta li ta"
  7 -> "ka di mi ta ka ta"
  _ -> error "Subdivisions greater than 7 are unsupported."

data Options = Options
  { beats :: Maybe Int
  , bpm :: Maybe Int
  , subdivision :: Maybe Subdivision
  } deriving (Generic, Show, Eq)

instance ParseRecord Options

data Config = Config
  { cBeats :: !Int
  , cBpm :: !Int
  , cSubdivision :: !Subdivision
  } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config 4 120 1

fromOpts :: Options -> Config
fromOpts Options{..} =
  Config (fromMaybe 4 beats) (fromMaybe 120 bpm) (fromMaybe 1 subdivision)

delaySeconds :: RealFrac n => n -> IO ()
delaySeconds n = threadDelay (round $ 1000000 * n)

say :: MonadIO io => [Text] -> Text -> io ExitCode
say args word = proc "say" (word:args) empty

afplay :: MonadIO io => Text -> io ExitCode
afplay file = proc "afplay" [file] empty

toWord :: Int -> Text
toWord 6 = "sick"
toWord 7 = "sev"
toWord 11 = "lev"
toWord n = pack (show n)

beatCycle :: Int -> Subdivision -> [Text]
beatCycle beats_ subdivision_ =
  intercalate subdivisionWords (pure <$> beats) ++ subdivisionWords
  where
    beats = map toWord [1..beats_]
    subdivisionWords = getSubdivisionWords subdivision_

-- | Do an action, then (asynchronously) wait the amount of time until the next
-- beat.
bpmWait :: Int -> IO a -> IO ()
bpmWait bpm_ io = do
  threadId <- forkIO (void io)
  delaySeconds (60 / fromIntegral bpm_ :: Double)
  killThread threadId

-- /tmp/.sounds/n-120-subdivision.aiff
fileName :: Config -> Text -> Text
fileName Config{..} word =
  "/tmp/.sounds/" <> word <> "-" <> conf <> ".aiff"
  where conf = pack (show cBpm) <> "-" <> pack (show cSubdivision)

genSounds :: Config -> [Text] -> IO SoundMap
genSounds config@Config{..} words_ = do
  exists <- doesDirectoryExist "/tmp/.sounds"
  unless exists $ mkdir "/tmp/.sounds"

  M.fromList <$> mapM sayFile (nub words_)
  where sayFile word = do
          let file = fileName config word
          void $ say ["--output-file=" <> file, "--rate=600"] word
          pure (word, file)

play :: Config -> [Text] -> SoundMap -> IO ()
play Config{..} words_ sounds =
  forM_ words_ $ \word ->
    case M.lookup word sounds of
      Nothing -> error "sound file not found!"
      Just sound_ -> bpmWait cBpm (forkIO . void $ afplay sound_)

-- The idea here will be to generate .aiff files from `say`
-- into a shared folder, then call afplay the-aiff-file when necessary
metronome_ :: Config -> ([Text] -> [Text]) -> IO ()
metronome_ config@Config{..} modifyWords = do
  let words_ = beatCycle cBeats cSubdivision
      bpm_ = cBpm * cSubdivision
  sounds <- genSounds config words_
  play config{ cBpm = bpm_ } (modifyWords words_) sounds

metronome1, metronome :: Config -> IO ()
metronome1 = (`metronome_` id)
metronome = (`metronome_` cycle)

-- Ok now to turn this into a useful program

main :: IO ()
main = do
  options_ <- getRecord "Metronome"
  let config = fromOpts options_
  metronome config
