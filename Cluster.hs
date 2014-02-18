module Cluster
where
  import Data.HashMap as H hiding (filter, map)
  import Data.List (find, sortBy, intersperse)
  import Data.Ord
  import Data.List.Split
  import Barcode
  import Sample
  
  
  data Cluster = Cluster { barcode    :: Barcode
                         , numSamples :: Int
                         , samples    :: [String]
                         , serialNum  :: Int
                         }
                         deriving Show
                         
  addToCluster :: String -> Cluster -> Cluster
  addToCluster name cluster = Cluster { barcode    = barcode cluster
                                      , numSamples = numSamples cluster + 1
                                      , samples    = name:(samples cluster)
                                      , serialNum  = serialNum cluster
                                      }

  clusterColor :: Cluster -> String
  clusterColor c = if any ambigLetter (barcode c)
                   then "red"
                   else "black"
                                      
  clusterMembers :: Cluster -> String
  clusterMembers n = reFormat $ samples n
                     
  reFormat :: [String] -> String
  reFormat xs = concat (fmtLine (take l xs)) ++ (reFormat' l (drop l xs))
                where l = ceiling $ sqrt $ fromIntegral $ length xs
                      reFormat' n [] = ""
                      reFormat' n xs' = concat (fmtLine (take l xs')) ++ reFormat' l (drop l xs')
                      fmtLine things = (intersperse " " things) ++ ["\n"]
  
  clusterLabel :: Cluster -> String
  clusterLabel n = barcode n ++ "\nsamples: " ++ (show $ numSamples n) ++ "\n" ++
                   clusterMembers n
  
  clusterToNode :: Cluster -> (Int, String)
  clusterToNode n = (serialNum n, clusterLabel n)

  findBySerial :: Int -> [Cluster] -> Maybe Cluster
  findBySerial n as = find (\c -> (serialNum c) == n) as

  nearbyClusters :: [Cluster] -> [(Int, Int)]
  nearbyClusters cs = [ (serialNum i, serialNum j)
                    | i <- cs
                    , j <- cs
                    , nearby i j
                    ]
                    where nearby a b = (serialNum a < serialNum b)
                                        && distNX (barcode a) (barcode b) <= 1
                                       
  mkCluster :: Sample -> Int -> Cluster
  mkCluster a n = Cluster (sampleBarcode a) 1 [sampleName a] n
                                      
  mkClusters :: [Sample] -> [Cluster]
  mkClusters []     = []
  mkClusters (a:as) = H.elems $ mkClusters' (singleton (sampleBarcode a) (mkCluster a 0)) as
                    where mkClusters' h (b:bs) = mkClusters' (insertWithKey
                                                  (\k n o -> addToCluster
                                                    (sampleName b) o) 
                                                  (sampleBarcode b) 
                                                  (mkCluster b (size h))
                                                  h)
                                                 bs
                          mkClusters' h [] = h
  
                                      
  cDist :: Cluster -> Cluster -> Int
  cDist a b = dist (barcode a) (barcode b)
  
  unambiguous :: [Cluster] -> [Cluster]
  unambiguous = filter check
              where check c = not $ any ambigLetter (barcode c)
              
  ambiguous :: [Cluster] -> [Cluster]
  ambiguous = filter check
            where check c = any ambigLetter (barcode c)
  
  -- want to be able to completely discard anything with >1 N, >5 X
  
  discard :: [Cluster] -> [Cluster]
  discard = filter check
          where check c =  ns <= 1 && xs + ns <= 6
                        where ns = count 'N' (barcode c)
                              xs = count 'X' (barcode c)
                              count x [] = 0
                              count x (a:as) = if x == a 
                                               then 1 + count x as
                                               else count x as
  
  -- hash on non-X/N-containing barcodes
  -- for X/N-containing barcodes, add to ALL matching clusters
  -- as possible matches!
  -- new fields for Cluster: numMaybeSamples, maybeSamples
  -- other funcs for Cluster: numAllSamples, allSamples
  
  -- new approach: first cluster only on non-X/N-containing barcodes
  -- then, drop barcodes with too many Xs or Ns
  -- then, add 'nearby' barcodes to clusters they might match
  -- compute neighboring clusters; create graph