module GTree where
import Board
import qualified System.Process as SP
import Gomoku

data GTree a = GNode { board :: a, player :: Cell, row :: Int, col :: Int,  score :: Int, forest :: [GTree a] } deriving Show
data MinMax = Min | Max deriving (Show,Eq)
countScore b cell = (checkEast b 1 1 cell 0 0) + (checkSouth b 1 1 cell 0 0) + (checkSouthEast b 15 1 cell 0 0)

initTree b p r c = GNode b p r c (countScore b p) []


maxDepth = 10
initList b p r c 
	|r >= 17 = []
	|c < 17 && isBlank b r c = [GNode (updateBoard b p r c) p r c (countScore (updateBoard b p r c) p) []] ++ initList b p (r+2) (c+2) 
	|c < 17 = initList b p (r+1) (c+3)	
	|r < 17 = initList b p (r+2) 1

buildTree depth (GNode b p r c s [])
	|depth < maxDepth = buildTree (depth+1) (GNode b p r c s (initList b (nextPlayer p) 1 1))
	|otherwise = GNode b p r c s []
buildTree depth (GNode b p r c s f) = GNode b p r c s (tmap buildTree depth f)

tmap fun arg [] = []
tmap fun arg list@(node@(GNode b p r c s []):nodes) = [fun arg node] ++ tmap fun arg nodes

ttmap fun arg1 [] arg2 = []
ttmap fun arg1 list@(node@(GNode b p r c s f):nodes) arg2 = [fun arg1 node arg2]++ttmap fun arg1 nodes arg2

	
findMin [] minVal = minVal
findMin list@(node@(GNode b p r c s f):nodes) minVal
	|minVal < s = findMin nodes minVal
	|otherwise = findMin nodes s
findMax [] maxVal = maxVal
findMax list@(node@(GNode b p r c s f):nodes) maxVal
	|maxVal > s = findMax nodes maxVal
	|otherwise = findMax nodes s

intoForest value [] = []
intoForest value list@(node@(GNode b p r c s f):nodes)
	|s == value = f
	|otherwise = intoForest value nodes

solve [] ro co res = (ro,co)
solve list@(node@(GNode b p r c s f):nodes) ro co res
	|minmax Max f 0 > res = solve nodes r c (minmax Max f 0)
	|otherwise = solve nodes ro co res
solution [] = []
solution list@(node@(GNode b p r c s f):nodes) = [minmax Max f 0] ++ solution nodes	
	
minmax mm list@(node@(GNode b p r c s []):nodes) result 
	|mm == Min = result - (findMax list 0)
	|mm == Max = result + (findMax list 0)
minmax mm list@(node@(GNode b p r c s f):nodes) result
	|mm == Min = minmax Max (intoForest (findMax list 0) list) (result - (findMax list 0))
	|mm == Max = minmax Min (intoForest (findMax list 0) list) (result + (findMax list 0)*2)

-------------------------------------------------------------------------------------------------
showG (GNode b pl r c s []) = showBoard b
showG (GNode b pl r c s f@(t:ts)) = showBoard b >> showG t >> showL ts
showL (x:xs) = showG x >> showL xs  
showL [] = putStrLn("-------------")
