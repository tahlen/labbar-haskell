module F2 where

data MolSeq = MolSeq { name :: String
                     , molSequence :: String
                     , dna :: Bool
                     } --deriving (Show)

              
-- filtrera ACTG, True om tom lista återstår
isDNA :: String -> Bool
isDNA x 
  | filter (\s -> not $ (s == 'A' || 
                         s == 'C' || 
                         s == 'T' || 
                         s == 'G')) x == "" = True 
  | otherwise = False

string2seq :: String -> String -> MolSeq
string2seq x y = MolSeq {name = x, molSequence = y, dna = (isDNA y)}

seqName :: MolSeq -> String
seqName (MolSeq {name = x}) = x

seqSequence :: MolSeq -> String
seqSequence (MolSeq {molSequence = x}) = x

seqLength :: MolSeq -> Int
seqLength x = length (seqSequence x)

seqDistance :: MolSeq -> MolSeq -> Double

