{-|
Module      : Data.String.Extra
Description : Extra utility functions for Strings.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : hjwylde@gmail.com

Extra utility functions for Strings.
-}

module Data.String.Extra (
    -- * Extra functions

    -- ** Utility
    sentence,
) where

import Data.List

sentence :: String -> [String] -> String
sentence _ [x]  = x
sentence sep xs = unwords [intercalate ", " (map show $ init xs), sep, show $ last xs]
