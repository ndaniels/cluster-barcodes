# cluster-barcodes
#### This is a simple tool for clustering genetic barcodes associated with named samples.

It assumes an input file which is tab-delimited, whose first field contains a 
sample identifier and whose second field contains a genetic barcode.
Bases labelled 'N' and 'X' represent 'no call' and 'multiple call' respectively.

Output is in DOT format, which can be read by Graphviz or Omnigraffle, among 
other tools.

Clusters are based on exact barcode matches; each node in the graph represents
a set of samples with identical barcodes. Nodes with ambiguous (containing 'X' 
or 'N') barcodes are colored red. Barcodes with >1 'N' or >5 ('X' + 'N') are 
dropped. Edges are drawn between nodes whose barcodes have a Hamming distance of
exactly 1 ('N' and 'X' entries do not count towards Hamming distance).

Two kinds of clusters are possible: by barcode (-c option), and by barcode/year 
(-l option, longitudinal clustering).

In longitudinal clustering, 'N' and 'X' entries DO count towards Hamming
distance.

Usage: `cluster-barcodes [-l|-c] input.txt output.dot`

#### Build instructions
* You must have the GHC haskell compiler. Haskell Platform is recommended.
* From the source directory, run `cabal install`