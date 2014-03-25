module Main
where

import System.Environment
import System.Exit
import Data.List
import Data.Ord
import Sample
import Cluster
import Graph

main :: IO ()

main = do
          [runType,inName,outName] <- getArgs >>= return . take 3
          if runType /= "-l" && runType /= "-c" 
            then putStrLn "Specify -l (longitudinal) or -c" >> exitFailure
            else return ()
          let longitudinal = if runType == "-l" then True else False
          f <- readFile inName
          let samples = map mkSample $ lines f
          
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