module Board where

data Cell = Blank | Black | White deriving (Eq,Show)

data Board a = Board [[a]] deriving (Show)
data Coords = Coords {x :: Int, y :: Int } deriving (Eq,Show)
initBoard :: Int -> Board Cell
initBoard x = Board (replicate x (replicate x Blank))

columns :: [[Cell]] -> Int -> IO ()
columns [] idy = putStrLn ""
columns (y:ys) idy 
		| idy < 10 = putStr ((show idy) ++ "  ") >> columns ys (idy + 1)
		| idy >= 10 = putStr ((show idy) ++ " ") >> columns ys (idy + 1)

rows :: [[Cell]] -> Int -> IO ()
rows [] idx = return ()
rows (x:xs) idx
		| idx < 10 = putStr (" " ++ (show idx)) >> showCell x idx >> rows xs (idx + 1)
		| idx >= 10 = putStr ((show idx) ++ "") >> showCell x idx >> rows xs (idx + 1)

showCell :: [Cell] -> Int -> IO ()
showCell [] idx 
	| idx < 10 = putStrLn (" " ++ (show idx))
	| idx >= 10 = putStrLn ((show idx) ++ "")
showCell (x:xs) idx 
		| x == Blank = putStr " _ " >> showCell xs idx
		| x == Black = putStr " X " >> showCell xs idx
		| x == White = putStr " O " >> showCell xs idx

showBoard :: Board Cell -> IO ()
showBoard (Board (x:xs)) = putStr "   " >> columns (x:xs) 1 >> rows (x:xs) 1 >> 
			   putStr "   " >> columns (x:xs) 1

