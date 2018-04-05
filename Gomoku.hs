module Gomoku where
	import Board
	import qualified System.Process as SP
	import Data.Tree
	--import System.Random
	clear :: IO ()
	clear = do
		_ <- SP.system "reset"
		return ()

	nextPlayer :: Cell -> Cell
	nextPlayer Black = White
	nextPlayer White = Black

	
	size = 19


	getElem :: Board Cell -> Int -> Int -> Cell
	getElem (Board x) row col 
		| row > 0 && row < (size+1) && col > 0 && col < (size +1) = x !! (row-1) !! (col-1)
		|otherwise = Blank
	
	getNorth :: Board Cell -> Int -> Int -> Cell
	getNorth (Board x) col row = x !! (row-2) !! (col-1)
	getSouth :: Board Cell -> Int -> Int -> Cell
	getSouth (Board x) col row = x !! (row) !! (col-1)
	getEast :: Board Cell -> Int -> Int -> Cell
	getEast (Board x) col row = x !! (row-1) !! (col)
	getSouthEast :: Board Cell -> Int -> Int -> Cell
	getSouthEast (Board x) col row = x !! (row) !! (col)
	getRow :: IO Int
	getRow = do
		putStr "Row index: "
		line <- getLine
		return (read line :: Int)

	getCol :: IO Int
	getCol = do
		putStr "Column index: "
		line <- getLine
		return (read line :: Int)
	
	--getRandom = randomRIO (1,19)
	isBlank :: Board Cell -> Int -> Int -> Bool
	isBlank (Board x) row col
		| (getElem (Board x) row col) == Blank = True 
		| (getElem (Board x) row col) == Black = False
		| (getElem (Board x) row col) == White = False 

	
	
	updateBoard :: (Board Cell) -> Cell -> Int -> Int -> (Board Cell)
	updateBoard (Board x) cell row col =
		Board (take (row-1) x ++
		[take (col-1) (x !! (row-1)) ++ [cell] ++ drop ((col-1) + 1) (x !! (row-1))] ++
		drop ((row-1) + 1) x)
	
	checkEast board row col cell counter tmpc
		|counter >= 5 = counter
		|col == 20 = checkEast board (row+1) 1 cell 0 (max tmpc counter)
		|row == 20 = tmpc
		|(getElem board row col) == cell = checkEast board row (col+1) cell (counter+1) tmpc
		|otherwise = checkEast board row (col+1) cell 0 (max tmpc counter)
	checkSouth board row col cell counter tmpc
		|counter >= 5 = counter
		|row == 20 = checkSouth board 1 (col+1) cell 0 (max tmpc counter)
		|col == 20 = tmpc
		|(getElem board row col) == cell = checkSouth board (row+1) col cell (counter+1) tmpc
		|otherwise = checkSouth board (row+1) col cell 0 (max tmpc counter)
	checkSouthEast board row col cell counter tmpc
		|counter >= 5 = counter
		|row == 1 && col == 16 = tmpc
		|col == 20 = checkSouthEast board 1 (22-row) cell 0 (max tmpc counter)
		|row == 20 = checkSouthEast board (20-col) 1 cell 0 (max tmpc counter)
		|(getElem board row col) == cell = checkSouthEast board (row+1) (col+1) cell (counter+1) tmpc
		|otherwise = checkSouthEast board (row+1) (col+1) cell 0 (max tmpc counter)
	checkAll board cell = ((checkEast board 1 1 cell 0 0)>=5) || ((checkSouth board 1 1 cell 0 0)>=5) || ((checkSouthEast board 15 1 cell 0 0)>=5)


