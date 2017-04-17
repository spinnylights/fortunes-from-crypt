module Fortunes
  ( fortune
  , fortunes) where

import Control.Monad
import Control.Monad.Random hiding (Random)
import System.Random.Shuffle
import PartsOfSpeech
type Random a = Rand StdGen a

intoYourLife :: Random String
intoYourLife = do
  adjs <- shuffleM adjectives
  let (adj1:adj2:adj3:_) = take 3 adjs
  noun1 <- noun
  nounWI <- nounWithInterior
  let preString = adj1 ++ ", " ++ adj2 ++ ", and " ++ adj3 ++ " " ++ noun1
                  ++ " will be coming into your " ++ nounWI ++ "."
  let vowels = ['a','e','i','o','u']
  if (head adj1) `elem` vowels
    then return ("An " ++ preString)
    else return ("A "  ++ preString)

aTruly :: Random String
aTruly = do
  adj <- adjective
  nonWI <- nounWithInterior
  nons <- shuffleM nouns
  let (non1:non2:_) = take 2 nons
  return ("A truly " ++ adj ++ " " ++ nonWI ++ " contains " ++ non1 ++
          " and " ++ non2 ++ " in abundance.")

beOffered :: Random String
beOffered = do
  adj <- adjective
  non <- noun
  return ("You will be offered a " ++ adj ++ " " ++ non ++ ". Say yes!")

soManyWays :: Random String
soManyWays = do
  adj <- adjective
  non <- noun
  return ("You are a " ++ adj ++ " " ++ non ++ " in so many ways.")

youWillEnjoy :: Random String
youWillEnjoy = do
  adj <- adjective
  nons <- shuffleM nouns
  let (non1:non2:_) = take 2 nons
  return ("You will enjoy " ++ adj ++ " " ++ non1 ++ "; you will be surrounded by " ++ non2 ++ ".")

fortunes :: [(Random String)]
fortunes = [ intoYourLife
           , aTruly
           , beOffered
           , soManyWays
           , youWillEnjoy]

fortune :: Random String
fortune = do
  forts <- shuffleM fortunes
  let (fort:_) = take 1 forts
  frt <- fort
  return frt
