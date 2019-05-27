module List where

import Prelude hiding (map, filter, zip, zipWith)

data List a = Cons a (List a)| Nil
            deriving (Show)

map :: List a -> (a -> b) -> List b
map Nil _ = Nil
map (Cons x xs) f = Cons (f x) (map xs f)

filter :: List a -> (a -> Bool) -> List a
filter Nil _ = Nil
filter (Cons x xs) f = if f x then Cons x (filter xs f) else filter xs f

foldLeft :: List a -> b -> (b -> a -> b) -> b
foldLeft Nil z _  = z
foldLeft (Cons x xs) z f = foldLeft xs (f z x) f

foldRight :: List a -> b -> (a -> b -> b) -> b
foldRight Nil z _ = z
foldRight (Cons x xs) z f = f x (foldRight xs z f)

append :: List a -> List a -> List a
append xs Nil = xs
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

flatMap :: List a -> (a -> List b) -> List b
flatMap xs f = foldRight (map xs f) Nil append

zip :: List a -> List b -> List (a,b)
zip _ Nil = Nil
zip Nil _ = Nil
zip (Cons x xs) (Cons y ys) = Cons (x,y) (zip xs ys)

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith _ _ Nil = Nil
zipWith _ Nil _ = Nil
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)

