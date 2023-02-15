module GameOfLife (life, glider) where

life :: Board -> IO ()
life board =
    do
        clearScreen
        showCells board
        wait 500000
        life (nextGeneration board)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

-- | Initial board sample.
glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

-- | Screen Utilities

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

type Position = (Int, Int)

-- | Displays a string at a given position by using control characters to move
-- the cursor to this position.
writeAt :: Position -> String -> IO ()
writeAt position xs = do
    goto position
    putStr xs

goto :: Position -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- | Game of Life

width :: Int
width = 10

height :: Int
height = 10

type Board = [Position]

showCells :: Board -> IO ()
showCells board = sequence_ [writeAt position "0" | position  <- board]

-- | Decide if a given position is alive or not.
isAlive :: Board -> Position -> Bool
isAlive board position = elem position board

isNotAlive :: Board -> Position -> Bool
isNotAlive board position = notElem position board

-- | Returns the neighbours of a position.
neighbours :: Position -> [Position]
neighbours (x, y) =
    map
        wrap
        [
            (x - 1, y - 1),
            (x, y - 1),
            (x + 1, y - 1),
            (x - 1, y),
            (x + 1, y),
            (x - 1, y + 1),
            (x, y + 1),
            (x + 1, y + 1)
        ]

-- | Takes account of the wrapping around at the edges of the board.
wrap :: Position -> Position
wrap (x, y) =
    (
        ((x - 1) `mod` width) + 1,
        ((y - 1) `mod` height) + 1
    )

-- | Calculates the number of live neighbours for a given position.
liveNeighbours :: Board -> Position -> Int
liveNeighbours board = length . filter (isAlive board) . neighbours

-- | Produce the list of living positions in a board that have precisely two or
-- three living neighbours, and hence survive to the next generation of the game.
survivors :: Board -> [Position]
survivors board =
    [position | position <- board, elem (liveNeighbours board position) [2, 3]]

-- | Produce a list of positions that can give birth to a new cell.
births :: Board -> [Position]
births board =
    [
        position | position <- rmdups (concatMap neighbours board),
        isNotAlive board position,
        liveNeighbours board position == 3
    ]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextGeneration :: Board -> Board
nextGeneration board = survivors board ++ births board
