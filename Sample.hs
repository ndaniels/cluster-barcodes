module Sample
where
  import Data.List.Split
  import Barcode
  
  data Sample = Sample { sampleName :: String, sampleBarcode :: Barcode}
              deriving Show
  
  mkSample :: String -> Sample
  mkSample s = mkSample' $ splitOn "\t" s
             where mkSample' (a:b:rest) = Sample a b
                   mkSample' (a:rest)   = error "Invalid line"
                   mkSample' []         = error "Invalid line"