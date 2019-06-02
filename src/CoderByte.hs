module CoderByte where

import Text.Regex (splitRegex, mkRegex)
import Data.Char

longest :: String -> Int
longest s = foldl max 0 (map (sum . map (const 1) . filter isAlphaNum) (splitRegex (mkRegex " ") s))


