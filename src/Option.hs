module Option where

import Prelude hiding (map, filter, zip, zipWith)

data Option a = Some a | None
                deriving (Show)

map :: Option a -> (a -> b) -> Option b
map None _ = None
map (Some a) f = Some (f a)

--flatMap :: Option a -> (a -> Option b) -> Option b
--flatMap None _ = None
--flatMap (Some a) f = f a

--printingOption =
--    let t = Some 12
--        m = map t (+2)
--        l = flatMap (Some 3) (\x -> map x (\y -> y + 2))
--     in print m
