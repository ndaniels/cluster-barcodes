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
  
  mkCytoNode :: Cluster -> String
  mkCytoNode c = (printf
                    "%s,%d,\"%s\"" ident size label)
                    where ident = show $ serialNum c
                          size  = celing $ sqrt $ fromIntegral $ numSamples c :: Int
                          label = clusterLabel c
  
  mkGraphEdge :: (Int,Int) -> String
  mkGraphEdge (a,b) = show a ++ " -- " ++ show b ++ ";\n"
  
  mkGraphEdgeWeighted :: (Int,Int,Int) -> String
  mkGraphEdgeWeighted (a,b,w) = show a ++ " -- " ++ show b ++ " [penwidth=" ++ show (4 - w) ++", label=" ++ show w ++"];\n"
    
  mkCytoEdgeWeighted :: (Int,Int,Int) -> String
  mkCytoEdgeWeighted (a,b,w) = show a ++ "," ++ show b ++ "," ++ show w
    
  mkGraph :: String -> [String] -> String
  mkGraph title ls = "graph " ++ title ++ " {\n" ++
                       (concat $ intersperse "\n" ls) ++
                       "\n }"
                       
  buildGraph :: String -> [String] -> [String] -> String
  buildGraph title nodes edges = mkGraph title (nodes ++ edges)
  
  buildLGraph :: String -> [String] -> [String] -> String
  buildLGraph title nodes edges = mkLGraph title (nodes ++ edges)
  
  mkLGraph :: String -> [String] -> String
  mkLGraph = undefined
    --
  -- let nonSingletons = filter (\c -> degree c (dateClusters clusters) >= 1) clusters
  -- let edgePairs = dateClusters nonSingletons
  -- let edgeStrings = map mkGraphEdge edgePairs
  -- let nodeStrings = map mkGraphNode nonSingletons
  -- let output = buildGraph "Barcodes" nodeStrings edgeStrings
  --
  
  
  -- mkLSubgraph :: [Cluster] -> String
  -- mkLSubgraph cs = "subgraph cluster_" ++ (show $ serialNum $ head cs) ++ "{\n"
  --                 ++ mkLNodeStrings cs
  --                 ++ mkLEdgeStrings cs
  --                 ++ "}"
                  
  -- mkLNodeStrings
                  
  mkLGraphEdge :: (Int,Int) -> String
  mkLGraphEdge (a,b) = show a ++ " -> " ++ show b ++ ";\n"
  
  -- use digraph here, -> edges
  -- subgraph cluster_1 { nodes and edges }