module F2 where

import Data.List

data MolSeq = MolSeq { name :: String
                     , molSequence :: String
                     , dna :: Bool
                     } --deriving (Show)

              
-- filtrera ACTG, returnera True om tom lista återstår
isDNA :: String -> Bool
isDNA x 
  | filter (\char -> not $ (   char == 'A' 
                            || char == 'C' 
                            || char == 'T' 
                            || char == 'G')) x == "" = True 
  | otherwise = False

string2seq :: String -> String -> MolSeq
string2seq name seq = MolSeq { name = name
                             , molSequence = seq
                             , dna = (isDNA seq)
                             }

seqName :: MolSeq -> String
seqName (MolSeq {name = x}) = x

seqSequence :: MolSeq -> String
seqSequence (MolSeq {molSequence = x}) = x

seqLength :: MolSeq -> Int
seqLength x = length (seqSequence x)

-- returnerar True om DNA, annars False
seqType :: MolSeq -> Bool
seqType (MolSeq {dna = x}) = x

normHamm' "" "" diff = diff
normHamm' (x:xs) (y:ys) diff = if x /= y 
                               then normHamm' xs ys (diff+1) 
                               else normHamm' xs ys diff

-- beräknar normaliserade Hamming-avståndet
-- förutsätter båda sekvenser samma längd
normHamm :: MolSeq -> MolSeq -> Double
normHamm seq1 seq2 = (normHamm' x y 0) / z
  where x = seqSequence seq1
        y = seqSequence seq2
        z = genericLength x


-- jämför enligt Jukes-Cantor
compareDNA :: Double -> Double
compareDNA x = (-3/4)*log(1-(4*x)/3)

-- jämför enligt Poisson
compareProtein :: Double -> Double
compareProtein x = (-19/20)*log(1-(20*x)/19)

-- jämför två sekvenser
seqDistance :: MolSeq -> MolSeq -> Double
seqDistance seq1 seq2 
  | (seqType seq1) && (seqType seq2)             = compareDNA (normHamm seq1 seq2)
  | (not $ seqType seq1) && (not $ seqType seq2) = compareProtein (normHamm seq1 seq2)
  | otherwise = error ("Sekvenserna ej av samma typ (endast DNA && DNA eller protein && protein tillåtet)")