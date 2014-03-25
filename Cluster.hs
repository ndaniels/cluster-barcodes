module Cluster
where
  import Data.HashMap as H hiding (filter, map)
  import Data.List (find, sortBy, intersperse)
  import Data.Ord
  import Data.List.Split
  import Barcode
  import Sample
  
  
  data Cluster = Cluster { barcode     :: Barcode
                         , numSamples  :: Int
                         , samples     :: [String]
                         , serialNum   :: Int
                         , clusterYear :: Int
                         }
                         deriving Show
                         
  addToCluster :: String -> Cluster -> Cluster
  addToCluster name cluster = Cluster { barcode     = barcode cluster
                                      , numSamples  = numSamples cluster + 1
                                      , samples     = name:(samples cluster)
                                      , serialNum   = serialNum cluster
                                      , clusterYear = clusterYear cluster
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
  clusterLabel n = (show $ clusterYear n) ++ " " ++ barcode n ++ "\nsamples: " 
                   ++ (show $ numSamples n) ++ "\n" ++
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
                                        
  degree :: Cluster -> [(Int, Int)] -> Int
  degree c edges = foldl isMember 0 edges
                 where isMember n e = if (fst e) == (serialNum c) ||
                                         (snd e) == (serialNum c)
                                      then n + 1
                                      else n
               
  dateClusters :: [Cluster] -> [(Int, Int)]
  dateClusters cs = [ (serialNum i, serialNum j)
                    | i <- cs
                    , j <- cs
                    , precedes i j cs
                    , shouldCluster i j
                    ]
                    where precedes :: Cluster -> Cluster -> [Cluster] -> Bool
                          precedes a b xs = (clusterYear a < clusterYear b)
                                            && (not $ any (\d -> isBetween a b d) xs)
                          isBetween a' b' d' = 
                               clusterYear a' < clusterYear d' 
                            && clusterYear b' > clusterYear d'
                          shouldCluster a b = dist2 (barcode a) (barcode b) == 0
                                       
  mkCluster :: (Sample -> String) -> Sample -> Int -> Cluster
  mkCluster key a n = Cluster (sampleBarcode a) 1 [sampleName a] n (sampleYear a)
                                      
  -- mkClusters groups samples with identical barcodes into a cluster.
  -- it relies on a hashmap, intially created with `singleton`, where a key is a
  -- barcode and a value is a Cluster. It relies on the insertWithKey function,
  -- which is a little complicated; the first argument to insertWithKey here is
  -- a lambda from key, old value, new value; it ignores key and new value
  -- and calls addToCluster to produce a new value which is the merged cluster.
  -- the second arg to insertWithKey is the key (barcode), while the third is
  -- the initialization for an empty hash bucket (mkCluster).
  -- thus, mkClusters' recurses on the barcodes (b:bs) while `updating' a hash
  -- (creating a new one based on the old one) using insertWithKey.
  mkClusters :: (Sample -> String) -> [Sample] -> [Cluster]
  mkClusters _ []     = []
  mkClusters key (a:as) = H.elems $ mkClusters' (singleton (key a) (mkCluster key a 0)) as
                        where mkClusters' h (b:bs) = mkClusters' (insertWithKey
                                                      (\k n o -> addToCluster
                                                        (sampleName b) o) 
                                                      (key b) 
                                                      (mkCluster key b (size h))
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
          where check c =  ns <= 1 && xs + ns <= 5
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