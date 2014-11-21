--- Sonnex, an alternative to Soundex for french language
--- Copyright (C) 2014 Frédéric BISSON

--- This program is free software: you can redistribute it and/or modify
--- it under the terms of the GNU General Public License as published by
--- the Free Software Foundation, either version 3 of the License, or
--- (at your option) any later version.

--- This program is distributed in the hope that it will be useful,
--- but WITHOUT ANY WARRANTY; without even the implied warranty of
--- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--- GNU General Public License for more details.

--- You should have received a copy of the GNU General Public License
--- along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
sonx :: String -> String -> String
-- End of recursive calls
sonx "" s = s

-- Apostroph is ignored/silent
sonx ('\'':cs) s = sonx cs s

-- Starting with 'a'
sonx "a" s = s ++ "a"
sonx "aient" s = s ++ "é"
sonx "ain" s = s ++ "1"
sonx ('a':'i':'n':v:cs) s
    | isVowel v = sonx (v:cs) (s ++ "é")
    | otherwise = sonx (v:cs) (s ++ "1")
sonx "ais" s = s ++ "é"
sonx ('a':'i':'s':v:cs) s
    | isVowel v = sonx (v:cs) (s ++ "éz")
    | otherwise = sonx (v:cs) (s ++ "és")
sonx "ail" s = s ++ "ai"
sonx ('a':'i':'l':'l':cs) s = sonx cs (s ++ "ai")
sonx ('a':'i':cs) s = sonx cs (s ++ "é")
sonx ('a':'m':'m':cs) s = sonx cs (s ++ "am")
sonx ('a':'m':c:cs) s
    | isVowel c = sonx (c:cs) (s ++ "am")
    | otherwise = sonx (c:cs) (s ++ "2")
sonx "an" s = s ++ "2"
sonx ('a':'n':'n':cs) s = sonx cs (s ++ "an")
sonx ('a':'n':c:cs) s
    | isVowel c = sonx (c:cs) (s ++ "an")
    | otherwise = sonx (c:cs) (s ++ "2")
sonx ('a':'s':'s':cs) s = sonx cs (s ++ "as")
sonx ('a':'s':c:cs) s
    | isConson c = sonx (c:cs) (s ++ "as")
    | otherwise  = sonx (c:cs) (s ++ "az")
sonx ('a':'u':cs) s = sonx cs (s ++ "o")
sonx "ay" s = s ++ "é"
sonx "ays" s = s ++ "é"

sonx ('à':cs) s = sonx cs (s ++ "a")
sonx ('â':cs) s = sonx cs (s ++ "a")

-- Starting with 'b'
sonx "b" s = s
sonx ('b':'b':cs) s = sonx cs (s ++ "b")

-- Starting with 'c'
sonx "c" s = s
sonx ('c':'a':cs) s = sonx ('a':cs) (s ++ "k")
sonx ('c':'c':cs) s = sonx cs (s ++ "ks")
sonx ('c':'e':cs) s = sonx ('e':cs) (s ++ "s")
sonx ('c':'\'':cs) s = sonx cs (s ++ "s")
sonx ('c':'h':'a':'o':cs) s = sonx ('a':'o':cs) (s ++ "k")
sonx ('c':'h':'l':cs) s = sonx cs (s ++ "kl")
sonx ('c':'h':'o':'e':cs) s = sonx ('o':'e':cs) (s ++ "k")
sonx ('c':'h':'œ':cs) s = sonx ('o':'e':cs) (s ++ "k")
sonx ('c':'h':'r':cs) s = sonx cs (s ++ "kr")
sonx ('c':'h':cs) s = sonx cs (s ++ "C")
sonx ('c':'i':cs) s = sonx ('i':cs) (s ++ "s")
sonx ('c':'k':cs) s = sonx cs (s ++ "k")
sonx ('c':'o':'e':'u':cs) s = sonx ('o':'e':'u':cs) (s ++ "k")
sonx ('c':'o':'m':'p':'t':cs) s = sonx cs (s ++ "k3t")
sonx ('c':'œ':'u':cs) s = sonx ('œ':'u':cs) (s ++ "k")
sonx ('c':'o':cs) s = sonx ('o':cs) (s ++ "k")
sonx ('c':'u':'e':'i':cs) s = sonx ('i':cs) (s ++ "ke")
sonx ('c':'u':cs) s = sonx ('u':cs) (s ++ "k")
sonx ('c':'y':cs) s = sonx ('y':cs) (s ++ "s")
sonx ('c':cs) s = sonx cs (s ++ "k")

sonx ('ç':cs) s = sonx cs (s ++ "s")

-- Starting with 'd'
sonx "d" s = s
sonx "ds" s = s
sonx ('d':'d':cs) s = sonx cs (s ++ "d")

-- Starting with 'e'
sonx "e" s = s
sonx "ec" s = s ++ "éc"
sonx "ef" s = s ++ "éf"
sonx "eaux" s = s ++ "o"
sonx ('e':'a':'n':'n':cs) s = sonx cs (s ++ "an")
sonx ('e':'a':'n':cs) s = sonx cs (s ++ "2")
sonx ('e':'a':'u':cs) s = sonx cs (s ++ "o")
sonx ('e':'f':'f':cs) s = sonx cs (s ++ "éf")
sonx "ein" s = s ++ "1"
sonx ('e':'i':'n':c:cs) s
    | isVowel c = sonx (c:cs) (s ++ "én")
    | otherwise = sonx (c:cs) (s ++ "1")
sonx ('e':'i':cs) s = sonx cs (s ++ "é")
sonx ('e':'l':'l':cs) s = sonx cs (s ++ "él")
sonx ('e':'l':c:cs) s
    | isConson c = sonx ('l':c:cs) (s ++ "é")
    | otherwise  = sonx ('l':c:cs) (s ++ "e")
sonx ('e':'m':'m':cs) s = sonx cs (s ++ "ém")
sonx ('e':'m':'p':cs) s = sonx cs (s ++ "2")
sonx ('e':'n':'n':cs) s = sonx cs (s ++ "én")
sonx "en" s = s ++ "2"
sonx ('e':'n':c:cs) s
    | isVowel c = sonx (c:cs) (s ++ "en")
    | otherwise = sonx (c:cs) (s ++ "2")
sonx "er" s = s ++ "é"
sonx "ert" s = s ++ "ér"
sonx ('e':'r':'r':cs) s = sonx cs (s ++ "ér")
sonx ('e':'r':'f':cs) s = sonx ('f':cs) (s ++ "ér")
sonx "es" s = s
sonx ('e':'s':'c':'h':cs) s = sonx cs (s ++ "éC")
sonx ('e':'s':'h':cs) s = sonx ('h':cs) (s ++ "é")
sonx ('e':'s':'n':cs) s = sonx ('n':cs) (s ++ "é")
sonx ('e':'s':'s':cs) s = sonx cs (s ++ "és")
sonx ('e':'s':c:cs) s
    | isConson c = sonx (c:cs) (s ++ "és")
    | otherwise  = sonx (c:cs) (s ++ "ez")
sonx ('é':'s':c:cs) s
    | isConson c = sonx cs (s ++ "és")
    | otherwise  = sonx (c:cs) (s ++ "éz")
sonx ('e':'t':'t':cs) s = sonx cs (s ++ "ét")
sonx "et" s = s ++ "é"
sonx ('e':'t':cs) s = sonx cs (s ++ "et")
sonx ('e':'u':'n':c:cs) s
    | isVowel c = sonx (c:cs) (s ++ "en")
    | otherwise = sonx (c:cs) (s ++ "1")
sonx "eux" s = s ++ "e"
sonx ('e':'u':'x':'i':cs) s = sonx ('i':cs) (s ++ "ez")
sonx ('e':'u':cs) s = sonx cs (s ++ "e")
sonx "ex" s = s ++ "éks"
sonx ('e':'y':c:cs) s
    | isConson c = sonx (c:cs) (s ++ "é")
    | otherwise  = sonx ('y':c:cs) (s ++ "é")
sonx "ez" s = s ++ "é"

sonx ('è':cs) s = sonx cs (s ++ "é")
sonx ('ê':cs) s = sonx cs (s ++ "é")
sonx ('ë':'l':cs) s = sonx ('l':cs) (s ++ "é")

-- Starting with 'f'
sonx ('f':'f':cs) s = sonx cs (s ++ "f")

-- Starting with 'g'
sonx "g" s = s
sonx ('g':'e':cs) s = sonx ('e':cs) (s ++ "j")
sonx ('g':'é':cs) s = sonx ('é':cs) (s ++ "j")
sonx ('g':'i':cs) s = sonx ('i':cs) (s ++ "j")
sonx ('g':'n':cs) s = sonx cs (s ++ "n")
sonx ('g':'y':cs) s = sonx ('y':cs) (s ++ "j")
sonx ('g':'u':'ë':cs) s = sonx cs (s ++ "gu")
sonx ('g':'u':cs) s = sonx cs (s ++ "g")
sonx ('g':'g':cs) s = sonx cs (s ++ "g")

-- Starting with 'h'
sonx ('h':cs) s = sonx cs s

-- Starting with 'i'
sonx "ic" s = s ++ "ik"
sonx "ics" s = s ++ "ik"
sonx ('i':'e':'n':'n':cs) s = sonx cs (s ++ "ién")
sonx ('i':'e':'n':cs) s = sonx cs (s ++ "i1")
sonx ('i':'n':'n':cs) s = sonx cs (s ++ "in")
sonx ('i':'n':'e':cs) s = sonx cs (s ++ "in")
sonx "in" s = s ++ "1"
sonx ('i':'n':c:cs) s
    | isVowel c = sonx (c:cs) (s ++ "in")
    | otherwise = sonx (c:cs) (s ++ "1")
sonx ('i':'s':c:cs) s
    | isConson c = sonx (c:cs) (s ++ "is")
    | otherwise  = sonx (c:cs) (s ++ "iz")
sonx ('i':'x':'i':cs) s = sonx ('i':cs) (s ++ "iz")
sonx ('i':'l':'l':cs) s = sonx cs (s ++ "i")
sonx ('i':cs) s = sonx cs (s ++ "i")

sonx ('ï':cs) s = sonx cs (s ++ "i")

-- Starting with 'l'
sonx ('l':'l':cs) s = sonx cs (s ++ "l")

-- Starting with 'm'
sonx ('m':'m':cs) s = sonx cs (s ++ "m")

-- Starting with 'n'
sonx ('n':'n':cs) s = sonx cs (s ++ "n")

-- Starting with 'o'
sonx ('o':'c':'c':cs) s = sonx cs (s ++ "ok")
sonx ('o':'e':'u':cs) s = sonx cs (s ++ "e")
sonx ('œ':'u':cs) s = sonx cs (s ++ "e")
sonx ('œ':cs) s = sonx cs (s ++ "e")
sonx "oient" s = s ++ "Ua"
sonx ('o':'i':'n':cs) s = sonx cs (s ++ "U1")
sonx ('o':'i':cs) s = sonx cs (s ++ "Ua")
sonx ('o':'m':'m':cs) s = sonx cs (s ++ "om")
sonx ('o':'m':c:cs) s
    | isVowel c = sonx (c:cs) (s ++ "om")
    | otherwise = sonx (c:cs) (s ++ "3")
sonx ('o':'n':'n':cs) s = sonx cs (s ++ "on")
sonx ('o':'n':cs) s = sonx cs (s ++ "3")
sonx ('o':'s':c:cs) s
    | isConson c = sonx cs (s ++ "os")
    | otherwise  = sonx (c:cs) (s ++ "oz")
sonx ('o':'u':cs) s = sonx cs (s ++ "U")
sonx ('o':'ù':cs) s = sonx cs (s ++ "U")
sonx ('o':'û':cs) s = sonx cs (s ++ "U")

sonx ('ô':cs) s = sonx cs (s ++ "o")
sonx ('ö':cs) s = sonx cs (s ++ "o")

-- Starting with 'p'
sonx "p" s = s
sonx ('p':'h':cs) s = sonx cs (s ++ "f")
sonx ('p':'p':cs) s = sonx cs (s ++ "p")
sonx ('p':'a':'y':'s':cs) s = sonx ('i':'s':cs) (s ++ "pé")

-- Starting with 'q'
sonx ('q':'u':'r':cs) s = sonx ('r':cs) (s ++ "ku")
sonx ('q':'u':cs) s = sonx cs (s ++ "k")
sonx ('q':cs) s = sonx cs (s ++ "k")

-- Starting with 'r'
sonx ('r':'r':cs) s = sonx cs (s ++ "r")

-- Starting with 's'
sonx "s" s = s
sonx ('s':'s':cs) s = sonx cs (s ++ "s")
sonx ('s':'c':'i':cs) s = sonx ('i':cs) (s ++ "s")

-- Starting with 't'
sonx "t" s = s
sonx ('t':'t':cs) s = sonx cs (s ++ "t")

-- Starting with 'u'
sonx "un" s = s ++ "1"
sonx ('û':cs) s = sonx cs (s ++ "u")

-- Starting with 'v'
-- 'v' can be handled by the generic case since there is no special thing
-- about this letter, it’s always pronounced 'v' and never doubled.

-- Starting with 'w'
sonx ('w':cs) s = sonx cs (s ++ "v")

-- Starting with 'x'
sonx "x" s = s
sonx ('x':'c':cs) s = sonx cs (s ++ "ks")
sonx ('x':c:cs) s
    | isVowel c = sonx (c:cs) (s ++ "kz")
    | otherwise = sonx (c:cs) (s ++ "ks")

-- Starting with 'y'
sonx ('y':cs) s = sonx cs (s ++ "i")

-- Starting with 'z'
sonx ('z':'z':cs) s = sonx cs (s ++ "z")

-- Copy every other character as is
sonx (c:cs) s = sonx cs (s ++ [c])

-- | Compute a Sonnex code for a french word.
--
-- The string must contain only one word.
-- The Sonnex code contains the following characters:
--     1 = un, ein, in, ain
--     2 = en, an
--     3 = on
--     a = a, à, â
--     b = b, bb
--     C = ch
--     d = d, dd
--     e = e, eu
--     é = ê, é, è, ai, ei
--     f = f, ff, ph
--     g = gu
--     i = î, i, ille
--     j = j, ge
--     k = k, c, qu, ck
--     l = l, ll
--     m = m, mm
--     n = n, nn
--     o = o, ô
--     p = p, pp
--     r = r, rr
--     s = s, ss
--     t = t, tt
--     u = u, ù, û
--     v = v, w
--     z = z, s
--     U = ou
--
-- Each character should be considered as being vocal/not silent
sonnex :: String -> String
sonnex word = sonx (map toLower word) ""

-- | Compute a Sonnex code for a french phrase.
--
-- It applies the sonnex function to each word in the phrase.
-- Since it uses the words/unwords couple, superfluous space character
-- are removed.
sonnexPhrase :: String -> String
sonnexPhrase phrase = unwords $ map sonnex (words phrase)

