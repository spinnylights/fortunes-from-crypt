module PartsOfSpeech
    ( adjective
    , adjectives
    , noun
    , nouns
    , sample
    , nounWithInterior
    ) where

import Control.Monad
import Control.Monad.Random hiding (Random)
type Random a = Rand StdGen a

sample :: [String] -> Random String
sample strings = (strings !!) `liftM` 
                 getRandomR (0, (length strings - 1))

adjectives = [ "lonely"
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
adjective = sample adjectives

nouns = [ "game"
        , "girl"
        , "crypt"
        , "gold coin"
        , "sensation"
        , "knife"
        , "veil"
        , "corridor"
        , "secret"
        , "world"
        , "truth"
        , "hell"
        , "vision"
        , "eyeball"
        , "horse"
        , "desert"
        , "desire"
        , "tunnel"
        , "village"
        , "bone"
        , "mouth"]

noun :: Random String
noun = sample nouns

nounsWithInterior = [ "life"
                   , "shack"
                   , "dreams"
                   , "catacombs"
                   , "hut"
                   , "grotto"
                   , "thoughts"
                   , "fantasies"]

nounWithInterior :: Random String
nounWithInterior = sample nounsWithInterior
