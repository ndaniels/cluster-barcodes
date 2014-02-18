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
          
          let edges = map mkGraphEdge $ nearbyClusters clusters
          
          -- let output = (map show clusters) ++ [show $ length clusters]
          
          let nodes = map mkGraphNode clusters
          
          let output = buildGraph "Barcode" nodes edges
          
          if outName == "-" 
            then putStrLn output
            else writeFile outName output