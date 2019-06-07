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
foldLeft (Cons x xs) z f = (foldLeft xs $! f z x) f -- 1,2,3,4 0  0 + 1 + 2 + 4 left to right

foldRight :: List a -> b -> (a -> b -> b) -> b
foldRight Nil z _ = z
foldRight (Cons x xs) z f = f x (foldRight xs z f) -- 1,2,3,4  f (1 : f(2 : f(3 : f(4 : 0)))) right to left

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

reduce :: (a -> a -> a) -> List a -> a
reduce f Nil = error "Illegal operation"
reduce f (Cons x Nil) = x
reduce f (Cons x xs) = f x (reduce f xs)

reduceL :: (a -> a -> a) -> List a -> a
reduceL f Nil = error "Illegal operation"
reduceL f (Cons x Nil) = x
reduceL f (Cons x (Cons y xs)) = reduceL f (Cons (f x y) xs)

