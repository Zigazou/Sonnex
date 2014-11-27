{-|
Module      : Sonnex
Description : Sonnex is an alternative to Soundex for french language
Copyright   : © 2014 Frédéric BISSON
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : alpha
Portability : POSIX

This package computes Sonnex codes for french words or phrases. It is an
alternative to the Soundex algorithm for french language.

= Characters of the Sonnex code

The Sonnex code contains the following characters:

- 1 ← un, ein, in, ain
- 2 ← en, an
- 3 ← on
- a ← a, à, â
- b ← b, bb
- C ← ch
- d ← d, dd
- e ← e, eu
- E ← ê, é, è, ai, ei
- f ← f, ff, ph
- g ← gu
- i ← î, i, ille
- j ← j, ge
- k ← k, c, qu, ck
- l ← l, ll
- m ← m, mm
- n ← n, nn
- o ← o, ô
- p ← p, pp
- r ← r, rr
- s ← s, ss
- t ← t, tt
- u ← u, ù, û
- v ← v, w
- z ← z, s
- U ← ou

The apostroph is ignored, every other character not understood by the Sonnex
algorthim is copied without changes.

= Examples

Here are a few examples of sonnex results:

>>> sonnex "champ"
C2

>>> sonnex "chant"
C2

>>> sonnex "boulot"
bUlo

>>> sonnex "bouleau"
bUlo

>>> sonnex "compte"
k3t

>>> sonnex "comte"
k3t

>>> sonnex "conte"
k3t

-}

module Text.Sonnex (sonnex, sonnexPhrase) where

import Data.Char

-- | Test is a character is to be considered a vowel
isVowel :: Char -> Bool
isVowel = (`elem` "aâàäeéèêëiîïoôöuùûüyœ")

-- | Test is a character is to be considered a conson
isConson :: Char -> Bool
isConson = (`elem` "bcçdfghjklmnpqrstvwxyz")

-- | Internal function computing a Sonnex code for a french word.
--
-- It requires its entry to be converted to lower case before being called.
sonx :: String -> String
-- End of recursive calls
sonx "" = ""

-- Apostroph is ignored/silent
sonx ('\'':cs) = sonx cs

-- Starting with 'a'
sonx "a" = "a"
sonx "aient" = "E"
sonx "ain" = "1"
sonx ('a':'i':'n':v:cs)
    | isVowel v = 'E':sonx (v:cs)
    | otherwise = '1':sonx (v:cs)
sonx "ais" = "E"
sonx ('a':'i':'s':v:cs)
    | v == 's'  = 'E':'s':sonx cs
    | isVowel v = 'E':'z':sonx (v:cs)
    | otherwise = 'E':'s':sonx (v:cs)
sonx "ail" = "ai"
sonx ('a':'i':'l':'l':cs) = 'a':'i':sonx cs
sonx ('a':'i':cs) = 'E':sonx cs
sonx ('a':'m':'m':cs) = 'a':'m':sonx cs
sonx ('a':'m':c:cs)
    | c == 'm'  = 'a':'m':sonx cs
    | isVowel c = 'a':'m':sonx (c:cs)
    | otherwise = '2':sonx (c:cs)
sonx "an" = "2"
sonx ('a':'n':c:cs)
    | c == 'n'  = 'a':'n':sonx cs
    | isVowel c = 'a':'n':sonx (c:cs)
    | otherwise = '2':sonx (c:cs)
sonx ('a':'s':c:cs)
    | c == 's'   = 'a':'s':sonx cs
    | isConson c = 'a':'s':sonx (c:cs)
    | otherwise  = 'a':'z':sonx (c:cs)
sonx ('a':'u':cs) = 'o':sonx cs
sonx "ay" = "E"
sonx "ays" = "E"

sonx ('à':cs) = 'a':sonx cs
sonx ('â':cs) = 'a':sonx cs

-- Starting with 'b'
sonx "b" = ""
sonx ('b':'b':cs) = 'b':sonx cs

-- Starting with 'c'
sonx "c" = ""
sonx ('c':'a':cs) = 'k':sonx ('a':cs)
sonx ('c':'c':v:cs)
    | v == 'o'  = 'k':sonx ('o':cs)
    | v == 'u'  = 'k':sonx ('u':cs)
    | otherwise = 'k':'s':sonx cs
sonx ('c':'e':cs) = 's':sonx ('e':cs)
sonx ('c':'\'':cs) = 's':sonx cs
sonx ('c':'h':'a':'o':cs) = 'k':sonx ('a':'o':cs)
sonx ('c':'h':'l':cs) = 'k':'l':sonx cs
sonx ('c':'h':'o':'e':cs) = 'k':sonx ('o':'e':cs)
sonx ('c':'h':'œ':cs) = 'k':sonx ('o':'e':cs)
sonx ('c':'h':'r':cs) = 'k':'r':sonx cs
sonx ('c':'h':cs) = 'C':sonx cs
sonx ('c':'i':cs) = 's':sonx ('i':cs)
sonx ('c':'k':cs) = 'k':sonx cs
sonx ('c':'o':'e':'u':cs) = 'k':sonx ('o':'e':'u':cs)
sonx ('c':'o':'m':'p':'t':cs) = 'k':'3':'t':sonx cs
sonx ('c':'œ':'u':cs) = 'k':sonx ('œ':'u':cs)
sonx ('c':'o':cs) = 'k':sonx ('o':cs)
sonx ('c':'u':'e':'i':cs) = 'k':'e':sonx ('i':cs)
sonx ('c':'u':cs) = 'k':sonx ('u':cs)
sonx ('c':'y':cs) = 's':sonx ('y':cs)
sonx ('c':cs) = 'k':sonx cs

sonx ('ç':cs) = 's':sonx cs

-- Starting with 'd'
sonx "d" = ""
sonx "ds" = ""
sonx ('d':'d':cs) = 'd':sonx cs

-- Starting with 'e'
sonx "e" = ""
sonx "ec" = "Ec"
sonx "ef" = "Ef"
sonx "eaux" = "o"
sonx ('e':'a':'n':'n':cs) = 'a':'n':sonx cs
sonx ('e':'a':'n':cs) = '2':sonx cs
sonx ('e':'a':'u':cs) = 'o':sonx cs
sonx ('e':'f':'f':cs) = 'E':'f':sonx cs
sonx ('e':'g':'m':cs) = 'E':sonx ('g':'m':cs)
sonx "ein" = "1"
sonx ('e':'i':'n':c:cs)
    | c == 'n'  = 'E':'n':sonx cs
    | isVowel c = 'E':'n':sonx (c:cs)
    | otherwise = '1':sonx (c:cs)
sonx ('e':'i':cs) = 'E':sonx cs
sonx ('e':'l':'l':cs) = 'E':'l':sonx cs
sonx ('e':'l':c:cs)
    | isConson c = 'E':sonx ('l':c:cs)
    | otherwise  = 'e':sonx ('l':c:cs)
sonx ('e':'m':'m':cs) = 'E':'m':sonx cs
sonx ('e':'m':'p':cs) = '2':sonx cs
sonx ('e':'n':'n':cs) = 'E':'n':sonx cs
sonx "en" = "2"
sonx ('e':'n':c:cs)
    | isVowel c = 'e':'n':sonx (c:cs)
    | otherwise = '2':sonx (c:cs)
sonx "er" = "E"
sonx "ert" = "Er"
sonx ('e':'r':'r':cs) = 'E':'r':sonx cs
sonx ('e':'r':'f':cs) = 'E':'r':sonx ('f':cs)
sonx "es" = ""
sonx ('e':'s':'c':'h':cs) = 'E':'C':sonx cs
sonx ('e':'s':c:cs)
    | c == 'h'   = 'E':sonx ('h':cs)
    | c == 'n'   = 'E':sonx ('n':cs)
    | c == 's'   = 'E':'s':sonx cs
    | isConson c = 'E':'s':sonx (c:cs)
    | otherwise  = 'e':'z':sonx (c:cs)
sonx ('é':'s':c:cs)
    | c == 's'   = 'E':'s':sonx cs
    | isConson c = 'E':'s':sonx (c:cs)
    | otherwise  = 'E':'z':sonx (c:cs)
sonx ('e':'t':'t':cs) = 'E':'t':sonx cs
sonx "et" = "E"
sonx ('e':'t':cs) = 'e':'t':sonx cs
sonx ('e':'u':'n':c:cs)
    | isVowel c = 'e':'n':sonx (c:cs)
    | otherwise = '1':sonx (c:cs)
sonx "eux" = "e"
sonx ('e':'u':'x':'i':cs) = 'e':'z':sonx ('i':cs)
sonx ('e':'u':cs) = 'e':sonx cs
sonx "ex" = "Eks"
sonx ('e':'y':c:cs)
    | isConson c = 'E':sonx (c:cs)
    | otherwise  = 'E':sonx ('y':c:cs)
sonx "ez" = "E"

sonx ('è':cs) = 'E':sonx cs
sonx ('ê':cs) = 'E':sonx cs
sonx ('ë':'l':cs) = 'E':sonx ('l':cs)

-- Starting with 'f'
sonx ('f':'f':cs) = 'f':sonx cs

-- Starting with 'g'
sonx "g" = ""
sonx ('g':'e':cs) = 'j':sonx ('e':cs)
sonx ('g':'é':cs) = 'j':sonx ('E':cs)
sonx ('g':'i':cs) = 'j':sonx ('i':cs)
sonx ('g':'n':cs) = 'n':sonx cs
sonx ('g':'y':cs) = 'j':sonx ('y':cs)
sonx ('g':'u':'ë':cs) = 'g':'u':sonx cs
sonx ('g':'u':cs) = 'g':sonx cs
sonx ('g':'g':cs) = 'g':sonx cs

-- Starting with 'h'
sonx ('h':cs) = sonx cs

-- Starting with 'i'
sonx "ic" = "ik"
sonx "ics" = "ik"
sonx ('i':'e':'n':'n':cs) = 'i':'E':'n':sonx cs
sonx ('i':'e':'n':cs) = 'i':'1':sonx cs
sonx "in" = "1"
sonx ('i':'n':c:cs)
    | c == 'n'  = 'i':'n':sonx cs
    | isVowel c = 'i':'n':sonx (c:cs)
    | otherwise = '1':sonx (c:cs)
sonx ('i':'s':c:cs)
    | c == 's'   = 'i':'s':sonx cs
    | isConson c = 'i':'s':sonx (c:cs)
    | otherwise  = 'i':'z':sonx (c:cs)
sonx ('i':'x':'i':cs) = 'i':'z':sonx ('i':cs)
sonx ('i':'l':'l':cs) = 'i':sonx cs
sonx ('i':cs) = 'i':sonx cs

sonx ('ï':cs) = 'i':sonx cs

-- Starting with 'l'
sonx ('l':'l':cs) = 'l':sonx cs

-- Starting with 'm'
sonx ('m':'m':cs) = 'm':sonx cs

-- Starting with 'n'
sonx ('n':'n':cs) = 'n':sonx cs

-- Starting with 'o'
sonx ('o':'c':'c':cs) = 'o':'k':sonx cs
sonx ('o':'e':'u':cs) = 'e':sonx cs
sonx ('œ':'u':cs) = 'e':sonx cs
sonx ('œ':cs) = 'e':sonx cs
sonx "oient" = "Ua"
sonx ('o':'i':'n':cs) = 'U':'1':sonx cs
sonx ('o':'i':cs) = 'U':'a':sonx cs
sonx ('o':'m':'m':cs) = 'o':'m':sonx cs
sonx ('o':'m':c:cs)
    | isVowel c = 'o':'m':sonx (c:cs)
    | otherwise = '3':sonx (c:cs)
sonx ('o':'n':'n':cs) = 'o':'n':sonx cs
sonx ('o':'n':cs) = '3':sonx cs
sonx ('o':'s':c:cs)
    | c == 's'   = 'o':'s':sonx cs
    | isConson c = 'o':'s':sonx (c:cs)
    | otherwise  = 'o':'z':sonx (c:cs)
sonx ('o':'u':cs) = 'U':sonx cs
sonx ('o':'ù':cs) = 'U':sonx cs
sonx ('o':'û':cs) = 'U':sonx cs

sonx ('ô':cs) = 'o':sonx cs
sonx ('ö':cs) = 'o':sonx cs

-- Starting with 'p'
sonx "p" = ""
sonx ('p':'h':cs) = 'f':sonx cs
sonx ('p':'p':cs) = 'p':sonx cs
sonx ('p':'a':'y':'s':cs) = 'p':'E':sonx ('i':'s':cs)

-- Starting with 'q'
sonx ('q':'u':'r':cs) = 'k':'u':sonx ('r':cs)
sonx ('q':'u':cs) = 'k':sonx cs
sonx ('q':cs) = 'k':sonx cs

-- Starting with 'r'
sonx ('r':'r':cs) = 'r':sonx cs

-- Starting with 's'
sonx "s" = ""
sonx ('s':'s':cs) = 's':sonx cs
sonx ('s':'c':'i':cs) = 's':sonx ('i':cs)

-- Starting with 't'
sonx "t" = ""
sonx ('t':'t':cs) = 't':sonx cs

-- Starting with 'u'
sonx "un" = "1"
sonx ('û':cs) = 'u':sonx cs
sonx ('u':'s':c:cs)
    | c == 's'   = 'u':'s':sonx cs
    | isConson c = 'u':'s':sonx (c:cs)
    | otherwise  = 'u':'z':sonx (c:cs)

-- Starting with 'v'
-- 'v' can be handled by the generic case since there is no special thing
-- about this letter, it’s always pronounced 'v' and never doubled.

-- Starting with 'w'
sonx ('w':cs) = 'v':sonx cs

-- Starting with 'x'
sonx "x" = ""
sonx ('x':c:cs)
    | c == 'c'  = 'k':'s':sonx cs
    | isVowel c = 'k':'z':sonx (c:cs)
    | otherwise = 'k':'s':sonx (c:cs)

-- Starting with 'y'
sonx ('y':cs) = 'i':sonx cs

-- Starting with 'z'
sonx ('z':'z':cs) = 'z':sonx cs

-- Copy every other character as is
sonx (c:cs) = c:sonx cs

-- | Compute a Sonnex code for a french word.
--
-- The string must contain only one word.
-- Each character should be considered as being vocal, not silent
--
-- prop> length (sonnex w) <= length w
sonnex :: String -> String
sonnex = sonx . (map toLower)

-- | Compute a Sonnex code for a french phrase.
--
-- It applies the sonnex function to each word in the phrase.
-- Since it uses the words/unwords couple, superfluous space character
-- are removed.
sonnexPhrase :: String -> String
sonnexPhrase phrase = unwords $ map sonnex (words phrase)

