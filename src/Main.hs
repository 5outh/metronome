{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

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

showText :: Show a => a -> Text
showText = pack . show

delaySeconds :: RealFrac n => n -> IO ()
delaySeconds n = threadDelay (round $ 1000000 * n)

say :: MonadIO io => [Text] -> Text -> io ExitCode
say args word = proc "say" (word:args) empty

afplay :: MonadIO io => Text -> io ExitCode
afplay file = proc "afplay" [file] empty

toWord :: Int -> Text
toWord 7 = "sev"
toWord 11 = "lev"
toWord 13 = "thirt"
toWord n = pack (show n)

timeCycle :: (Int, Int) -> Bool -> [Text]
timeCycle time_@(num, _) ands_ =
  (if ands_ then (++ ["n"]) . intersperse "n" else id) beats
  where beats = map toWord [1..num]

cycleDo :: [IO a] -> IO ()
cycleDo = sequence_ . cycle

-- | Do an action, then (asynchronously) wait the amount of time until the next
-- beat.
-- I think "say" is taking too long to start or something?
bpmWait :: Int -> IO a -> IO ()
bpmWait bpm_ io = do
  threadId <- forkIO (void io)
  delaySeconds (60 / fromIntegral bpm_)
  killThread threadId

metronome :: (Text -> IO a) -> Config -> IO ()
metronome fn Config{..} = do
  let words_ = timeCycle time ands
      -- TODO: could obviously be improved
      bpm_ = if ands then bpm * 2 else bpm
  mapM_ (bpmWait bpm_ . fn) words_

printMetronome :: Config -> IO ()
printMetronome = metronome T.putStrLn

sayMetronome :: Config -> IO ()
sayMetronome config@Config{..} =
  metronome (say ["--rate=" <> rate]) config
  where rate = pack (show $ bpm * 3)

  -- /tmp/.sounds/n-120-ands?.aiff
fileName :: Config -> Text -> Text
fileName Config{..} word =
  mconcat [ "/tmp/.sounds/"
          , word
          , "-"
          , showText bpm
          , if ands then "-ands" else ""
          , ".aiff"
          ]

ignoreFailures :: IO () -> IO ()
ignoreFailures io = io `catch` handler
  where
    handler :: SomeException -> IO ()
    handler = const $ pure ()

genSounds :: Config -> [Text] -> IO SoundMap
genSounds config@Config{..} words_ = do
  -- Make the directory for the sounds to go in
  ignoreFailures $ mkdir "/tmp/.sounds"

  -- Write a .aiff file with the given sounds (permissions problems?)
  pairs <- forM (nub words_) $ \word -> do
    let file = fileName config word
        rate = pack (show $ bpm * 3)
    code <- say ["--output-file=" <> file, "--rate=" <> rate] word
    pure (word, file)

  pure $ M.fromList pairs

cyclePlay :: Config -> [Text] -> SoundMap -> IO ()
cyclePlay config@Config{..} words_ sounds =
  forM_ (cycle words_) $ \word -> do
    let sound = M.lookup word sounds
    case sound of
      Nothing -> error "sound file not found!"
      Just sound_ -> bpmWait bpm (forkIO . void $ afplay sound_)

-- The idea here will be to generate .aiff files from `say`
-- into a shared folder, then call afplay the-aiff-file when necessary
realMetronome :: Config -> IO ()
realMetronome config@Config{..} = do
  let words_ = timeCycle time ands
      -- TODO: could obviously be improved
      bpm_ = if ands then bpm * 2 else bpm
  sounds <- genSounds config words_
  cyclePlay config{ bpm = bpm_ } words_ sounds

main :: IO ()
main = realMetronome defaultConfig
