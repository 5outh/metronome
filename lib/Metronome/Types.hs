{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
module Metronome.Types where

import Options.Generic
import qualified Data.Map as M

type SoundMap = M.Map Text Text
type Subdivision = Int

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

data SayEnv = SayEnv
  { saySounds :: SoundMap
  , sayWords :: [Text]
  , sayConfig :: Config
  } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config 4 120 1


