module F2 where

import Data.List

class Evol a where
  distance :: a -> a -> Double
  name :: a -> String          
  
data MolSeq = MolSeq   { msName        :: String
                       , molSequence   :: String
                       , msDNA         :: Bool
                       } deriving (Show)

  
data Profile = Profile { pName	       :: String
                       , pMatrix       :: [[(Char, Int)]]
                       , pDNA          :: Bool
                       , sequences     :: Int
                       } deriving (Show)

instance Evol MolSeq where
 distance a b = seqDistance a b
instance Evol Profile where
 distance a b = profileDistance a b
              
-- filtrera ACTG, returnera True om tom lista återstår
-- förutsätter att ett protein inte endast består av ACTG
isDNA :: String -> Bool
isDNA x 
  | filter (\char -> not $ (   char == 'A' 
                            || char == 'C' 
                            || char == 'T' 
                            || char == 'G')) x == "" = True 
  | otherwise = False

string2seq :: String -> String -> MolSeq
string2seq name seq = MolSeq { msName = name
                             , molSequence = seq
                             , msDNA = (isDNA seq)
                             }

seqName :: MolSeq -> String
seqName (MolSeq {msName = x}) = x

seqSequence :: MolSeq -> String
seqSequence (MolSeq {molSequence = x}) = x

seqLength :: MolSeq -> Int
seqLength x = length (seqSequence x)

-- returnerar True om DNA, annars False
seqType :: MolSeq -> Bool
seqType (MolSeq {msDNA = x}) = x

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
  | (seqType seq1) && (seqType seq2) = compareDNA (normHamm seq1 seq2)
  | (not $ seqType seq1) && (not $ seqType seq2) = compareProtein (normHamm seq1 seq2)
  | otherwise = error ("Sekvenserna ej av samma typ (endast DNA && DNA eller protein && protein tillåtet)")
                
nucleotides = "ACGT"
aminoacids = sort "ARNDCEQGHILKMFPSTWYVX"

makeProfileMatrix :: [MolSeq] -> [[(Char, Int)]]
makeProfileMatrix [] = error "Empty_sequence_list"
makeProfileMatrix sl = res 
  where
    t = seqType (head sl)
    defaults = 
      if t then -- om DNA
        zip nucleotides (replicate (length nucleotides) 0) -- skapa lista med tupler för varje nukleotid 
      else  	-- om protein
        zip aminoacids (replicate (length aminoacids) 0)   -- skapa lista med tupler för varje aminosyra
    strs = map seqSequence sl                              -- Rad (iii)
    tmpl = map (map (\x -> ((head x), (length x))) . group . sort) 
           (transpose strs)                                -- Rad (iv)
    equalFst a b = (fst a) == (fst b)
    res = map sort (map (\l -> unionBy equalFst l defaults) tmpl)

profileName :: Profile -> String
profileName (Profile {pName = x}) = x

profileMatrix :: Profile -> [[(Char, Int)]]
profileMatrix (Profile {pMatrix = x}) = x

profileSequences :: Profile -> Int
profileSequences (Profile {sequences = x}) = x

molseqs2profile :: String -> [MolSeq] -> Profile
molseqs2profile name molseqs = Profile { pName	   = name
		     	       	       , pMatrix   = matrix
				       , pDNA	   = t
				       , sequences = seqs
				       }
      	where 
      	    matrix = makeProfileMatrix molseqs
      	    t      = seqType (head molseqs)
      	    seqs   = length molseqs

profileFrequency :: Profile -> Int -> Char -> Double
profileFrequency p i c = (fromIntegral . snd . head . filter f $ (matrix !! i)) / seqs
	where
	    f = (\x -> fst x == c)
	    matrix = profileMatrix p
	    seqs = fromIntegral (profileSequences p)

profileDistance :: Profile -> Profile -> Double
profileDistance m1 m2 = sum [profileDistance' m1 m2 j | j <- aminoacids]
profileDistance' m1 m2 j = sum [abs ((profileFrequency m1 i j) - (profileFrequency m2 i j)) | i <- [0..(length (profileMatrix m1))-1]]

