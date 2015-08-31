
{-|
Module      : Language.Qux.Version
Description : Haskell constants of the language version.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Haskell constants of the language version.
-}

module Language.Qux.Version where

import Data.List (intercalate)


-- * Version

-- | The version formatted as "major.minor.patch".
version = intercalate "." (map show [major, minor, patch])


-- ** Components

-- | The major component.
major = 0

-- | The minor component.
minor = 0

-- | The patch component.
patch = 0

