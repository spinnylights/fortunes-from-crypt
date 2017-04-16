module Lib
    ( someFunc
    ) where

import PartsOfSpeech
import Control.Monad.Random

someFunc :: IO ()
someFunc = do
  adj <- evalRandIO adjective
  putStrLn adj
