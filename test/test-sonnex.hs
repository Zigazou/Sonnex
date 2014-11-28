{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : TestSonnex
Description : Tests for HaMinitel
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : alpha
Portability : POSIX

Tests for Sonnex
-}

module Main where

import           Control.Monad       (unless)
import           System.Exit         (exitFailure)

import           Test.QuickCheck
import           Test.QuickCheck.All (quickCheckAll)

import           Text.Sonnex

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)

homonymsList :: [[String]]
homonymsList =
    [ [ "amande", "amende"]
    , [ "ancre", "encre" ]
    , [ "autel", "hôtel" ]
    , [ "balade", "ballade" ]
    , [ "basilic", "basilique"]
    , [ "boulot", "bouleau" ]
    , [ "cane", "canne" ]
    , [ "censé", "sensé" ]
    , [ "compte", "comte", "conte" ]
    , [ "cygne", "signe" ]
    , [ "date", "datte" ]
    , [ "dessin", "dessein" ]
    , [ "différend", "différent"]
    , [ "du", "dû" ]
    , [ "filtre", "philtre" ]
    , [ "flan", "flanc" ]
    , [ "foi", "foie", "fois" ]
    , [ "gène", "gêne" ]
    , [ "golf", "golfe" ]
    , [ "héraut", "héro", "héros" ]
    , [ "lacer", "lasser" ]
    , [ "lire", "lyre" ]
    , [ "maire", "mer", "mère" ]
    , [ "mal","mâle", "malle" ]
    , [ "mite", "mythe" ]
    , [ "pain", "pin" ]
    , [ "palais", "palet" ]
    , [ "a", "à" ]
    , [ "au", "aux", "haut" ]
    --, [ "cerf", "serf", "serre" ]
    , [ "choeur", "chœur", "cœur", "coeur" ]
    , [ "cahos", "chaos" ]
    , [ "ce", "se" ]
    , [ "ai", "es", "est", "et" ]
    , [ "cric", "crique" ]
    , [ "on", "ont" ]
    , [ "champ", "chant" ]
    , [ "ou", "où" ]
    --, [ "c'est", "s'est", "ces", "ses", "sais", "sait" ]
    , [ "col", "colle" ]
    --, [ "mes", "mais", "m'est", "mets", "met" ]
    , [ "colon", "côlon" ]
    , [ "son", "sont", "sons" ]
    --, [ "court", "cours", "courent", "cour", "courre" ]
    , [ "ta", "t'a", "tas" ]
    , [ "dans", "d'en", "dent" ]
    , [ "ma", "m'a", "m'as", "mas", "mât" ]
    , [ "mon", "m'ont", "mont" ]
    , [ "la", "l'a", "l'as", "là" ]
    , [ "au", "aux", "haut" ]
    , [ "leur", "leurs", "l'heure", "leurre" ]
    , [ "sa", "ça", "çà" ]
    --, [ "les", "l'ai", "l'es", "l'est", "laid", "lait", "laie" ]
    , [ "peu", "peut", "peux" ]
    , [ "quand", "quant", "qu'en" ]
    , [ "ni", "n'y", "nie", "nies", "nid", "nids" ]
    , [ "ton", "t'ont", "thon" ]
    , [ "sans", "s'en", "sang", "cent" ]
    , [ "tant", "temps", "tend", "tends", "t'en" ]
    , [ "sois", "soit", "soient", "soie" ]
    , [ "quelque", "quelques" ]
    , [ "mur", "mûr", "mûre", "mure" ]
    , [ "dis", "dit", "dix" ]
    --, [ "quel", "quels", "quelle", "quelles", "qu'elle", "qu'elles" ]
    , [ "tous", "tout", "toux" ]
    , [ "si", "s'y", "scie" ]
    ]

prop_homonyms = forAll (elements homonymsList) $ \homonyms ->
    allTheSame (map sonnex homonyms)

return []

main = do
    allPass <- $quickCheckAll
    unless allPass exitFailure
