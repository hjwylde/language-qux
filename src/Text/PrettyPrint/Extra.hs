{-|
Module      : Text.PrettyPrint.Extra
Description : Extra utility functions for pretty printing.

Copyright   : (c) Henry J. Wylde, 2017
License     : BSD3
Maintainer  : hjwylde@gmail.com

Extra utility functions for pretty printing.
-}

module Text.PrettyPrint.Extra (
    -- * Rendering
    pShow,

    -- * Separators
    dot, dcolon, rarrow,

    -- * Operators
    asterisk, bang, dequals, fslash, hyphen, langle, percent, plus, rangle,
) where

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

-- | @pShow a@ pretty prints @a@ using a rending mode of 'OneLineMode'.
pShow :: Pretty a => a -> String
pShow = renderStyle (style { mode = OneLineMode }) . pPrint

dot :: Doc
dot = char '.'

dcolon :: Doc
dcolon = text "::"

rarrow :: Doc
rarrow = text "->"

asterisk :: Doc
asterisk = char '*'

bang :: Doc
bang = char '!'

dequals :: Doc
dequals = text "=="

fslash :: Doc
fslash = char '/'

hyphen :: Doc
hyphen = char '-'

langle :: Doc
langle = char '<'

percent :: Doc
percent = char '%'

plus :: Doc
plus = char '+'

rangle :: Doc
rangle = char '>'
