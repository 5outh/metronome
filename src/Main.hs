{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Directory
import Control.Exception
import Control.Concurrent
import Control.Monad
import Data.List
import Turtle hiding (time)
import Data.Text (pack)
import qualified Data.Text.IO as T
import qualified Data.Map as M

type SoundMap = M.Map Text Text

data Config = Config
  { time :: !(Int, Int)
  , bpm :: !Int
  , ands :: !Bool
  } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config (4, 4) 120 True

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

timeCycle :: (Int, Int) -> Bool -> [Text]
timeCycle (num, _) ands_ =
  if ands_ then intersperse "n" beats ++ ["n"] else beats
  where beats = map toWord [1..num]

-- | Do an action, then (asynchronously) wait the amount of time until the next
-- beat.
bpmWait :: Int -> IO a -> IO ()
bpmWait bpm_ io = do
  threadId <- forkIO (void io)
  delaySeconds (60 / fromIntegral bpm_)
  killThread threadId

-- /tmp/.sounds/n-120-ands?.aiff
fileName :: Config -> Text -> Text
fileName Config{..} word =
  "/tmp/.sounds/" <> word <> "-" <> conf <> ".aiff"
  where conf = pack (show bpm) <> "-" <> if ands then "-a" else ""

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
      Just sound_ -> bpmWait bpm (forkIO . void $ afplay sound_)

-- The idea here will be to generate .aiff files from `say`
-- into a shared folder, then call afplay the-aiff-file when necessary
metronome_ :: Config -> ([Text] -> [Text]) -> IO ()
metronome_ config@Config{..} f = do
  let words_ = timeCycle time ands
      bpm_ = if ands then bpm * 2 else bpm
  sounds <- genSounds config words_
  play config{ bpm = bpm_ } (f words_) sounds

metronome1, metronome :: Config -> IO ()
metronome1 = (`metronome_` id)
metronome = (`metronome_` cycle)

test = do
  forM_ [40,60..400] $ \bpm_ -> do
    metronome1 defaultConfig{ bpm=bpm_ }

main :: IO ()
main = metronome defaultConfig
