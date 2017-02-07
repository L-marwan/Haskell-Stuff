import Data.List
import System.IO
import Data.String
import Data.Char

frequency :: Ord a => [a] -> [(Int,a)] 
frequency l = zip (map length $ group l) $ nub l
nFreq n l  = print $take n (reverse $ sort $ frequency l)


getInt s = putStrLn s >> getLine >>= return . read:: IO Int
getString s = putStrLn s >> getLine >>= return . read ::IO String



resultat n path = do
	contents <- readFile path 
	nFreq n (words contents)


main = do 
	num <- getInt "Donner le nombre de resultats:"
	path <- getString "Donner le chemin du fichier :" 
	print (resultat num path)	
