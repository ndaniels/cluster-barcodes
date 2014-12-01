{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, 
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables, NamedFieldPuns,
             NoMonomorphismRestriction #-}
module Tabs
       ( parseTabs
       )
where

import Data.Ix
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.String
import Control.Applicative
import Numeric
import qualified Text.Parsec.Token as P
import Data.Char
import Data.Either

import Samples

tabFile :: GenParser Char () Samples
tabFile = do -- 
        samples <- many sampleLine
        
sampleLine = Sample <$> sName <*> sBarcode <*> sYear
        
  data Sample = Sample { sampleName :: String
                       , sampleBarcode :: Barcode
                       , sampleYear :: Int}
              deriving Show
  
  mkSample :: String -> Sample
  mkSample s = mkSample' $ splitOn "\t" s
             where mkSample' (a:b:rest) = Sample a b (getYear a)
                   mkSample' (a:rest)   = error "Invalid line"
                   mkSample' []         = error "Invalid line"
                   getYear :: String -> Int
                   getYear s' = (read $ ((splitOn "." s') !! 1))
                   
sName =            
                   
hmmNodeGroup = HMMNode <$> hmmNodeNum <*>hmmMatchEmissions <*> hmmInsertionEmissions <*>
                           hmmTransitions

-- this is SOMETHING to an Int. 
hmmNodeNum = optSpaces *> (try (const 0 <$> string "COMPO") <|> p_int) <* optSpaces 
           -- where comp = optional (string "COMPO") -- string "COMP" <|> string "" -- ugly. better way?