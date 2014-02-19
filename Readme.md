# cluster-barcodes
#### This is a simple tool for clustering genetic barcodes associated with named samples.

It assumes an input file which is tab-delimited, whose first fiend contains a sample identifier and whose second field contains a genetic barcode.
Bases labelled 'N' and 'X' represent 'no call' and 'multiple call' respectively.

Output is in DOT format, which can be read by Graphviz or Omnigraffle, among other tools.

Usage: `cluster-barcodes input.txt output.dot`

#### Build instructions
* You must have the GHC haskell compiler. Haskell Platform is recommended.
* From the source directory, run `cabal install`