
module Language.Qux.Version where

import Data.List


version = intercalate "." (map show [major, minor, patch, release])

major = 0
minor = 0
patch = 0
release = 0

