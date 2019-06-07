module Main where
import Prelude hiding (map, filter, length, zip, zipWith)
import Lib
import List
import Variables
import CoderByte
import Option hiding (map)

main :: IO ()
main = do
        print (select "apple")
        print (length [1,2,3,4])
        print (mean [1,2,3,4])
        printingList
        print (longest "hello@ there! yes")
        print (firstFactorial 4)
        print (firstReverse "abcd")
        printingList


printingList =
  let l1 = Cons 1 (Cons 2 (Cons 3 (Cons 3 Nil)))
      l2 = Cons 4 (Cons 5 (Cons 6 Nil))
      ll = Cons l1 (Cons l2 Nil)
      mm = map l1 (* 2)
      ff = filter l1 (> 1)
      fl = foldLeft l "Z" (++)
      fr = foldRight l "Z" (++)
      fmi = flatMap ll id
      fm = flatMap ll (`Cons` Nil)
      l = Cons "a" (Cons "b" (Cons "c" Nil))
      zp = zip l1 l2
      zw = zipWith (*) l1 l2
      rdi = reduce (+) (Cons 2 Nil)
      rdl = reduceL (+) l2
   in print mm >> print ff >> print fl >> print fr >> print fmi >> print ll >> print fmi >> print fm >> print zp >>
      print zw >> print rdi >> print rdl







