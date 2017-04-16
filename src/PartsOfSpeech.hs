module PartsOfSpeech
    ( adjective
    ) where

import Control.Monad
import Control.Monad.Random hiding (Random)
type Random a = Rand StdGen a

adjectives = [ "banal"
             , "lonely"
             , "thin"
             , "infinite"
             , "horrible"
             , "screaming"
             , "shadowy"
             , "unholy"
             , "hidden"
             , "sad"
             , "beautiful"
             , "large"
             , "violet"
             , "inescapable"
             , "dark"
             , "heavy"
             , "lingering"]

adjective :: Random String
adjective = (adjectives !!) `liftM` getRandomR (0, (length adjectives - 1))

--nouns = [ "game"
--        , "girl"
--        , "crypt"
--        , ]
