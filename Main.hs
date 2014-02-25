module Main
where

import System.Environment
import Data.List
import Data.Ord
import Sample
import Cluster
import Graph

main :: IO ()

main = do
          [inName,outName] <- getArgs >>= return . take 2
          f <- readFile inName
          let samples = map mkSample $ lines f
          -- let (goods, bads) = prune $ mkClusters samples
          let clusters = sortBy (comparing numSamples) $ discard $ mkClusters samples
          
          -- discard degree-1 nodes, unless they contain >1 samples.
          let nonSingletons = filter (\c -> degree c (nearbyClusters clusters) >= 1 
                                     || numSamples c > 1) clusters
          
          -- let edgePairs = nearbyClusters clusters
          
          let edgePairs = nearbyClusters nonSingletons
          
          let edgeStrings = map mkGraphEdge edgePairs
          
          -- let nodeStrings = map mkGraphNode clusters
          
          -- let output = (map show clusters) ++ [show $ length clusters]
          
          
          let nodeStrings = map mkGraphNode nonSingletons
          
          let output = buildGraph "Barcode" nodeStrings edgeStrings
          
          if outName == "-" 
            then putStrLn output
            else writeFile outName output