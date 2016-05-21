{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Metronome.Say where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Data.List
import qualified Data.Map                   as M
import           Data.Text                  (pack)
import qualified Data.Text                  as T
import           Metronome.Common
import           Metronome.Types
import           Options.Generic
import           System.Directory
import           Turtle                     hiding (time)

beatCycle :: Int -> Subdivision -> [Text]
beatCycle beats_ subdivision_ =
  intercalate subdivisionWords (pure <$> beats) ++ subdivisionWords
  where
    beats = map toWord [1..beats_]
    subdivisionWords = getSubdivisionWords subdivision_

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

sayMetronome :: ReaderT SayEnv IO ()
sayMetronome = do
  SayEnv{..} <- ask
  liftIO $ forM_ sayWords $ \word ->
    case M.lookup word saySounds of
      Nothing -> error "sound file not found!"
      Just sound_ -> bpmWait (cBpm sayConfig) (forkIO . void $ afplay sound_)

initSayMetronome :: Config -> IO SayEnv
initSayMetronome config@Config{..} = do
  let words_ = beatCycle cBeats cSubdivision
      bpm_ = cBpm * cSubdivision
  sounds <- genSounds config words_
  pure (SayEnv sounds (cycle words_) config{ cBpm = bpm_ })

say :: MonadIO io => [Text] -> Text -> io ExitCode
say args word = proc "say" (word:args) empty

afplay :: MonadIO io => Text -> io ExitCode
afplay file = proc "afplay" [file] empty

toWord :: Int -> Text
toWord 6 = "sick"
toWord 7 = "sev"
toWord 11 = "lev"
toWord n = pack (show n)

getSubdivisionWords :: Subdivision -> [Text]
getSubdivisionWords = T.words . \case
  1 -> ""
  2 -> "n"
  3 -> "ta tee"
  4 -> "e n uh"
  5 -> "ka ta ka ta"
  6 -> "ta la ta li ta"
  7 -> "ka di mi ta ka ta"
  _ -> error "Subdivisions greater than 7 are unsupported."


