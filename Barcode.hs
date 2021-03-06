module Barcode

where
  
  type Barcode = [Char]
  
  ambigLetter :: Char -> Bool
  ambigLetter x = x == 'N' || x == 'X'
  
  dist :: Barcode -> Barcode -> Int
  dist [] [] = 0
  dist (a:as) [] = error "Unequal length lists"
  dist [] (b:bs) = error "Unequal length lists"
  dist (a:as) (b:bs) = dist' a b + dist as bs
       where dist' x y = if x == y
                         then 0
                         else 1

  dist2 :: Barcode -> Barcode -> Int
  dist2 as bs = foldr (+) 0 $ map check $ zip as bs
        where check (x,y) = if x == y
                            then 0
                            else 1
                            
  distNX :: Barcode -> Barcode -> Int
  distNX as bs = foldr (+) 0 $ map check $ zip as bs
        where check (x,y) = if x == y || ambigLetter x || ambigLetter y
                            then 0
                            else 1