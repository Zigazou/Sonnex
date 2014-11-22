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

module Main where

import Control.Monad
import Text.Sonnex

-- | You can use this file to test the Sonnex functions
-- You can use it like this:
--     cat test-sonnex-homonymes | runhaskell test-sonnex.hs
main = forever $ do
    line <- getLine
    putStrLn (line ++ " → " ++ sonnexPhrase line)

