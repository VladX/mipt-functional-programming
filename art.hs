art = ["@..@..................",
       "..@..@................",
       "....@..@..@@@@@@@@@@@@",
       "......@..@............",
       "........@..@..........",
       "......@..@...@..@@@@@@",
       "....@..@.......@......",
       "..@..@...........@....",
       "@..@...............@.."]

flipVer = reverse -- Вертикальное отражение
flipHor = map reverse -- Горизонтальное отражение
rotate180 = flipVer . flipHor -- поворот на 180
rotateCw = flipHor . transpose -- По часовой на 90
rotateCcw = flipVer . transpose -- Против часовой на 90

transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((x : xs) : xss) = (x : [h | (h : _) <- xss]) : transpose (xs : [ t | (_ : t) <- xss])

main = do
	putStrLn "Original art:\n"
	mapM_ putStrLn art
	putStrLn "\nflipVer:\n"
	mapM_ putStrLn (flipVer art)
	putStrLn "\nflipHor:\n"
	mapM_ putStrLn (flipHor art)
	putStrLn "\nrotate180:\n"
	mapM_ putStrLn (rotate180 art)
	putStrLn "\nrotateCw:\n"
	mapM_ putStrLn (rotateCw art)
	putStrLn "\nrotateCcw:\n"
	mapM_ putStrLn (rotateCcw art)