module F1 where

import Data.List
import Data.Char


--- #1 ---

{-  beräkna efter matematiska definitionen
    dålig optimering, tar flera minuter att räkna ut n=40 -}
fibShit :: Integer -> Integer
fibShit 0 = 0
fibShit 1 = 1
fibShit n = (fibShit (n-1)) + (fibShit (n-2))

{- fib x beräknas genom att skapa en lista [0, 1]
   skicka till hjälpfunktionen fib'
   skicka vidare listan rekursivt efter addition [a, b] -> [b, a+b]
   upprepa x-1 tills x==2 och returnera till sist a+b -}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib' n [0, 1]
fib' 2 [a, b] = a+b
fib' n [a, b] = fib' (n-1) [b, (a+b)]


--- #2 ---

{- om ej vokal, dubblera bokstaven med 'o' emellan
    med denna implementation räknas även ex. mellanslag och siffror som icke-vokaler -}
isVowel :: Char -> Bool
isVowel x 
  | x == 'a' || x == 'A' = True
  | x == 'e' || x == 'E' = True
  | x == 'i' || x == 'I' = True
  | x == 'o' || x == 'O' = True
  | x == 'u' || x == 'U' = True 
  | x == 'y' || x == 'Y' = True
  | otherwise       = False

rovarsprak' :: Char -> String
rovarsprak' x = if not (isVowel x)
            then [x, 'o' ,x]
            else [x]
                 
rovarsprak :: String -> String
rovarsprak x = concat (map rovarsprak' x)

-- tag char, om ej vokal droppa två nästa chars
karpsravor :: String -> String
karpsravor (x:xs) = if (isVowel x)
                     then x:(karpsravor xs)
                     else x:(karpsravor (drop 2 xs))
karpsravor [] = []

{-   allt som inte är bokstäver blir mellanslag
     funktionen words delar upp
     medellängd = (antal bokstäver) / (antal ord) -}
medellangd' x = sum y / fromIntegral (genericLength y)
  where y = map genericLength x

--- #3 ---

isAlpha' x = if isAlpha x then x else ' '

medellangd x = medellangd' (words $ map isAlpha' x)


--- #4 ---

{-  skapa två listor 
    lista 1: vartannat elem från första elem (1,3,5,7 etc.)
    lista 2: vartannat elem från andra elem (2,4,6,8 etc.)
    addera första listan och resultatet av skyffla på andra listan -}
skyffla :: [a] -> [a]
skyffla [a] = [a]
skyffla xs = (varannan1 xs) ++ (skyffla (varannan2 xs))

varannan1 xs = case xs of
  (y:ys) -> y : varannan1 (drop 1 ys)
  []     -> []
  
varannan2 xs = case drop 1 xs of
  (y:ys) -> y : varannan2 ys
  []     -> []