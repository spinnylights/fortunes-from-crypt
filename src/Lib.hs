module Lib
    ( someFunc
    ) where

import Fortunes
import Control.Monad
import Control.Monad.Random

someFunc :: IO ()
someFunc = do
  fort <- evalRandIO fortune
  putStrLn fort
