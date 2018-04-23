module Main where
	import Board
	import Gomoku
	import GTree
	check r c = r > 0 && r < 20 && c > 0 && c < 20

	loopW :: Board Cell -> IO ()
	loopW (Board x) = do
		if checkAll (Board x) Black
		then do
			clear >> putStrLn "THE END !! PLAY AGAIN" >>  main
			else do
				r <- getRow				
				c <- getCol
				
				if (check r c && isBlank (Board x) r c )
				then do
					loopB (updateBoard (Board x) White r c) r c
					else
						loopW (Board x) 

	loopB :: Board Cell -> Int -> Int -> IO ()
	loopB (Board x) r c = do
		if checkAll (Board x) White
		then do
			clear >> putStrLn "THE END !! PLAY AGAIN" >>  main
			else do
			let tree = tmap buildTree 1 $ initList (Board x) Black 1 1
			let coords = solve tree (r+1) (c+1) 0
			let r = fst coords		
			let c = snd coords
			clear
			showBoard (updateBoard (Board x) Black r c)
			loopW (updateBoard (Board x) Black r c)

	main = do
		putStrLn ""
		let board = initBoard 19 
		showBoard board
		loopW board
		return ()
	
