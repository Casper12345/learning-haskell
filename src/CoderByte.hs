module CoderByte where

import Text.Regex (splitRegex, mkRegex)
import Data.Char

longest :: String -> Int
longest s = foldl max 0 (map (sum . map (const 1) . filter isAlphaNum) (splitRegex (mkRegex " ") s))

firstFactorial :: Int -> Int
firstFactorial i = if i == 0 then 1 else i * firstFactorial (i - 1)

firstReverse :: String -> String
firstReverse = foldl (\b a -> a : b) ""



