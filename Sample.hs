module Sample
where
  import Data.List.Split
  import Barcode
  
  data Sample = Sample { sampleName :: String
                       , sampleBarcode :: Barcode
                       , sampleYear :: Int}
              deriving Show
  
  mkSample :: String -> Sample
  mkSample s = mkSample' $ splitOn "\t" s
             where mkSample' (a:b:rest) = Sample a b (getYear a)
                   mkSample' (a:rest)   = error "Invalid line"
                   mkSample' []         = error "Invalid line"
                   getYear :: String -> Int
                   getYear s' = (read $ ((splitOn "." s') !! 1))
