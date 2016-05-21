{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Trans.Reader
import           Metronome.Say
import           Metronome.Types
import           Options.Generic

main :: IO ()
main = do
  options_ <- getRecord "Metronome"
  let config = fromOpts options_
  initSayMetronome config >>= runReaderT sayMetronome
