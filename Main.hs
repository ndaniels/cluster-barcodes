module Main
where

import System.Environment
import System.Exit
import Data.List
import Data.Ord
import Sample
import Cluster
import Graph
import Util

main :: IO ()

main = do
          [runType,inName,outName] <- getArgs >>= return . take 3
          if runType /= "-l" && runType /= "-c" && runType /= "-w" 
            then putStrLn "Specify -l (longitudinal), -w (weighted) or -c" >> exitFailure
            else return ()
          let longitudinal = if runType == "-l" then True else False
          let weighted = if runType == "-w" then True else False
          f <- readFile inName
          let samples = if longitudinal then
                           map mkSampleYear $ splitLines f
                        else
                           map mkSample $ splitLines f
          
          let clusters = if longitudinal then
                            sortBy (comparing numSamples) $ discard $ 
                              mkClusters 
                              (\x -> sampleBarcode x ++ (show $ sampleYear x)) 
                              samples
                          else
                            sortBy (comparing numSamples) $ discard $ 
                              mkClusters 
                              sampleBarcode samples
          if longitudinal 
            then do
              -- discard degree-0 nodes
              let nonSingletons = filter (\c -> degree c (dateClusters clusters) >= 1) clusters
              let edgePairs = dateClusters nonSingletons
              let edgeStrings = map mkGraphEdge edgePairs
              let nodeStrings = map mkGraphNode nonSingletons
              let output = buildGraph "Barcodes" nodeStrings edgeStrings
              -- draw edges between adjacent ones.
              if outName == "-" 
                then putStrLn output
                else writeFile outName output
            else if weighted
              then do
                let nonSingletons = filter (\c -> degree c (nearbyClusters clusters) >= 1 
                                           || numSamples c > 1) clusters
                let edgePairs = nearbyClustersWeighted nonSingletons 3
                let edgeStrings = map mkGraphEdgeWeighted edgePairs
                let nodeStrings = map mkGraphNode nonSingletons
                let output = buildGraph "Barcode" nodeStrings edgeStrings
                if outName == "-" 
                  then putStrLn output
                  else writeFile outName output  
            else do
              -- discard degree-0 nodes, unless they contain >1 samples.
              let nonSingletons = filter (\c -> degree c (nearbyClusters clusters) >= 1 
                                         || numSamples c > 1) clusters
              let edgePairs = nearbyClusters nonSingletons  
              let edgeStrings = map mkGraphEdge edgePairs
              let nodeStrings = map mkGraphNode nonSingletons
              let output = buildGraph "Barcode" nodeStrings edgeStrings
              if outName == "-" 
                then putStrLn output
                else writeFile outName output