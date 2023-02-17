module UnbeatableTicTacToe (pvp) where

import Data.Char
import Data.List
import System.IO

-- PvP
pvp :: IO ()
pvp = runGame buildEmptyGrid O

-- | PvE 
-- TODO: fix game start
pve :: IO ()
pve = runGamePve buildEmptyGrid O

-- | Represents a player in the game, which can be O, B or X. B represents
-- a blank cell that has not yet been occupied by X or O.
data Player = O  -- ^ A player with O symbol
            | B  -- ^ A blank cell that has not yet been occupied by X or O.
            | X  -- ^ A player with X symbol
            deriving (Eq, Ord, Show)

-- | Represents a tree data structure, where each node has a value of type 'a'
-- and a list of child nodes, also of type 'a'.
data Tree a = Node a  -- ^ A node in the tree with a value of type 'a'
              [Tree a]  -- ^ A list of child nodes, also of type 'a'
              deriving Show

-- | Represents a grid in the game, which is a two-dimensional list of players.
type Grid = [[Player]]

-- | Represents a position on the game grid, which is a tuple of two integers
-- representing row and column.
type Position = (Int, Int)

-- | The size of the game grid (3x3).
gridSize :: Int
gridSize = 3

-- | The maximum search depth for the game tree.
maxTreeDepth :: Int
maxTreeDepth = 9

-- | Clears the console screen.
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- | Creates an empty grid (filled with blank cells).
buildEmptyGrid :: Grid
buildEmptyGrid = replicate gridSize (replicate gridSize B)

-- | Given a player, returns the next player in turn.
nextPlayer :: Player -> Player
nextPlayer O = X
nextPlayer B = B
nextPlayer X = O

-- | Checks if the given grid is full, i.e., if there are no blank cells left in it.
isGridFull :: Grid -> Bool
isGridFull = notElem B . concat

-- | Decide who is the current player by comparing the number of O's and X's in a flatten grid.
currentPlayer :: Grid -> Player
currentPlayer grid = if totalOs <= totalXs then O else X
    where
        totalOs = length (filter (== O) flattenGrid)
        totalXs = length (filter (== X) flattenGrid)
        flattenGrid = concat grid

-- | Returns a boolean indicating if the specified player has won on the given grid.
isWinner :: Player -> Grid -> Bool
isWinner player grid = any line (rows ++ cols ++ diagonals)
    where
        line = all (== player)
        rows = grid
        cols = transpose grid
        diagonals = [getMainDiagonal grid, getMainDiagonal (map reverse grid)]

-- | Returns the main diagonal of the given grid.
getMainDiagonal :: Grid -> [Player]
getMainDiagonal grid = [grid !! n !! n | n <- [0..gridSize-1]]

-- | Returns a boolean indicating if either player has won on the given grid.
hasWon :: Grid -> Bool
hasWon grid = isWinner O grid || isWinner X grid

-- | Prints the given grid to the console.
putGrid :: Grid -> IO ()
putGrid =
    putStrLn . unlines . concat . interLeave bar . map showRow
    where
        bar = [replicate ((gridSize * 4) - 1) '-']

-- | Converts a row of players into a list of strings representing the row.
showRow :: [Player] -> [String]
showRow = beside . interLeave bar . map showPlayer
    where
        beside = foldr1 (zipWith (++))
        bar = replicate 3 "|"

-- | Converts a single player into a list of strings representing the player.
showPlayer :: Player -> [String]
showPlayer B = ["   ", "   ", "   "]
showPlayer O = ["   ", " O ", "   "]
showPlayer X = ["   ", " X ", "   "]

-- | Intersperses a separator value between the elements of a list.
interLeave :: a -> [a] -> [a]
interLeave x [] = []
interLeave x [y] = [y]
interLeave x (y:ys) = y : x : interLeave x ys

-- | Checks if a move is valid by ensuring that the target index is within the
-- game grid and is an empty cell.
isMoveValid :: Grid -> Int -> Bool
isMoveValid grid targetIndex =
    -- | Check if the target index is in the right range.
    0 <= targetIndex && targetIndex < gridSize ^ 2
    -- | Check if the target index value is a blank cell.
    && concat grid !! targetIndex == B

-- | Generates a list of all possible next game states given the current grid,
-- target index, and player.
generateNextStates :: Grid -> Int -> Player -> [Grid]
generateNextStates grid targetIndex player =
    [divideIntoChunks gridSize (before ++ [player] ++ after) | isMoveValid grid targetIndex]
    where
        (before, _:after) = splitAt targetIndex (concat grid)

-- | Divides a list into sublists of the given size.
divideIntoChunks :: Int -> [a] -> [[a]]
divideIntoChunks n [] = []
divideIntoChunks n xs = take n xs : divideIntoChunks n (drop n xs)

-- | Reads a natural number from the standard input. If the input is not a
-- natural number, displays an error message and prompts again.
getNat :: String -> IO Int
getNat promptForMove = do
    xs <- getLine
    if xs /= [] && all isDigit xs then
        return (read xs)
    else do
        putStrLn "Error: Invalid number."
        getNat promptForMove

-- | Runs the game with the given grid and player.
runGame :: Grid -> Player -> IO ()
runGame grid player = do
    clearScreen
    moveCursorTo (1, 1)
    putGrid grid
    checkGameOver grid player

-- | Moves the cursor to the given position in the grid.
moveCursorTo :: Position -> IO ()
moveCursorTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- | Checks if the game is over and displays the winner if there is one.
checkGameOver :: Grid -> Player -> IO ()
checkGameOver grid player
    | isWinner O grid = putStrLn "Player O wins!\n"
    | isWinner X grid = putStrLn "Player X wins!\n"
    | isGridFull grid = putStrLn "It's a draw!\n"
    | otherwise = do
        targetPosition <- getNat (promptForMove player)
        case generateNextStates grid targetPosition player of
            [] -> do
                putStrLn "Error: Invalid move."
                checkGameOver grid player
            [nextGrid] -> runGame nextGrid (nextPlayer player)

-- | Prompts the player to enter a generateNextStates.
promptForMove :: Player -> String
promptForMove player = "Player " ++ show player ++ ", enter your move: "

-- | Builds a game tree for all possible moves from the current grid state
-- for a given player.
buildGameTree :: Grid -> Player -> Tree Grid
buildGameTree grid player =
    Node grid [buildGameTree nextGrid (nextPlayer player) | nextGrid <- possibleMoves grid player]

-- | Returns a list of all possible moves that a player can make on a given grid,
-- excluding any grids that have already been hasWon or are full.
possibleMoves :: Grid -> Player -> [Grid]
possibleMoves grid player
    | hasWon grid = []
    | isGridFull grid = []
    | otherwise = concat [generateNextStates grid cellIndex player | cellIndex <- [0..((gridSize ^ 2) - 1)]]

-- | Prunes a tree to a given depth by recursively creating a new tree that
-- contains only the nodes up to the given depth.
pruneTreeToDepth :: Int -> Tree a -> Tree a
pruneTreeToDepth 0 (Node key _) = Node key []
pruneTreeToDepth n (Node key subtrees) =
    Node key [pruneTreeToDepth (n - 1) subtree | subtree <- subtrees]


-- | Implementation of the minimax algorithm to determine the optimal next generateNextStates.
minMax :: Tree Grid -> Tree (Grid, Player)
minMax (Node grid [])
    | isWinner O grid = Node (grid, O) []
    | isWinner X grid = Node (grid, X) []
    | otherwise = Node (grid, B) []
minMax (Node grid children)
    | currentPlayer grid == O = Node (grid, minimum childScores) childNodes
    | currentPlayer grid == X = Node (grid, maximum childScores) childNodes
        where
            childNodes = map minMax children
            childScores = [score | Node (_, score) _ <- childNodes]

-- | Given a current game state and player, returns the optimal next generateNextStates for the player.
optimalNextMove :: Grid -> Player -> Grid
optimalNextMove currentGrid currentPlayer =
    head [nextGrid | Node (nextGrid, nextPlayer) _ <- subTrees, nextPlayer == best]
    where
        prunedTree = pruneTreeToDepth maxTreeDepth (buildGameTree currentGrid currentPlayer)
        Node (_, best) subTrees = minMax prunedTree

runGamePve :: Grid -> Player -> IO ()
runGamePve grid player = do
    clearScreen
    moveCursorTo (1, 1)
    putGrid grid
    checkGameOverPve grid player

checkGameOverPve :: Grid -> Player -> IO ()
checkGameOverPve grid player
    | isWinner O grid = putStrLn "Player O wins!\n"
    | isWinner X grid = putStrLn "Player X wind!\n"
    | isGridFull grid = putStrLn "It's a draw!\n"
    | player == O = do
        targetPosition <- getNat (promptForMove player)
        case generateNextStates grid targetPosition player of
            [] -> do
                putStrLn "Error: Invalid move."
                checkGameOver grid player
            [nextGrid] -> runGamePve nextGrid (nextPlayer player)
    | player == X = do
        putStr "Player X is thinking... "
        runGamePve $! optimalNextMove grid player nextPlayer player
