module Graph
where
  import Data.List
  import Text.Printf
  import Cluster
  
  mkSize :: Int -> Double
  mkSize x = sqrt $ fromIntegral x

  mkGraphNode :: Cluster -> String
  mkGraphNode c = (printf 
                      "node [shape=circle, width=%d, label=\"%s\", color=\"%s\"]; %s;\n" 
                       size label color ident)
                      where size = ceiling $ sqrt $ fromIntegral $ numSamples c :: Int
                            label = clusterLabel c
                            color = clusterColor c
                            ident = show $ serialNum c
  
  mkGraphEdge :: (Int,Int) -> String
  mkGraphEdge (a,b) = show a ++ " -- " ++ show b ++ ";\n"
    
  mkGraph :: String -> [String] -> String
  mkGraph title ls = "graph " ++ title ++ " {\n" ++
                       (concat $ intersperse "\n" ls) ++
                       "\n }"
                       
  buildGraph :: String -> [String] -> [String] -> String
  buildGraph title nodes edges = mkGraph title (nodes ++ edges)