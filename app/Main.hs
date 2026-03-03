module Main where
import Data.Char (toLower, digitToInt)
import Control.Concurrent (threadDelay)
import System.Exit (exitSuccess)

data Piece = Empty | Player | Bot | PlayerKing | BotKing
  deriving (Eq, Show)

type Board = [[Piece]]

-- initial board setup, that will be updated with each later move
initialBoard :: Board
initialBoard = [
    [Empty, Bot, Empty, Bot, Empty, Bot, Empty, Bot],
    [Bot, Empty, Bot, Empty, Bot, Empty, Bot, Empty],
    [Empty, Bot, Empty, Bot, Empty, Bot, Empty, Bot],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Player, Empty, Player, Empty, Player, Empty, Player, Empty],
    [Empty, Player, Empty, Player, Empty, Player, Empty, Player],
    [Player, Empty, Player, Empty, Player, Empty, Player, Empty]
  ]

-- displaying the board, which should update with each move
displayBoard :: Board -> IO ()
displayBoard board = do
    -- column labels (for readability)
    putStrLn "  A B C D E F G H"
    -- row labels (for readaibility)
    mapM_ printRow (zip [1..] board)
  where
    -- printing each row to the IO
    printRow :: (Int, [Piece]) -> IO ()
    printRow (rowNum, row) = do
        putStrLn (show rowNum ++ " " ++ concatMap showPiece row)
    -- translating each piece into a string to show in IO
    showPiece :: Piece -> String
    showPiece Empty = ". "
    showPiece Player = "x "
    showPiece Bot = "o "
    showPiece PlayerKing = "X "
    showPiece BotKing = "O "

-- receives player input from terminal
getPlayerMove :: IO ((Int, Int), (Int, Int))
getPlayerMove = do
    putStrLn "Enter move (ex. C6 B5 for C6 -> B5) or type 'quit' to end game: "
    move <- getLine
    case map toLower move of
        "quit" -> do
            putStrLn "You quit the game! Rerun the program if you'd like to play again."
            exitSuccess
        _ -> do
            let ws = words move
            if length ws /= 2
                then invalidInput
                else do
                    let srcStr = head ws
                        dstStr = ws !! 1
                    if validPos srcStr && validPos dstStr
                        then return (parsePosition srcStr, parsePosition dstStr)
                        else invalidInput
  where
    invalidInput = do
        putStrLn "Invalid input, try again."
        getPlayerMove 

-- checks if a position input string is valid 
validPos :: String -> Bool
validPos [col,row] =
    let c = toLower col
    in c >= 'a' && c <= 'h' && row >= '1' && row <= '8'
validPos _ = False

-- convers the board position received from getPlayerMove into a (x, y) tuple
parsePosition :: String -> (Int, Int)
parsePosition pos = (row, col)
  where
    row = digitToInt (pos !! 1) - 1  -- changes row character to have proper position in 2D array since in arrays, we start at 0
    col = fromEnum (toLower (head pos)) - fromEnum 'a'  -- changes column character to have proper position, with A as 0, B as 1, and so on

-- checks whether the input is valid
isValidMove :: Board -> (Int, Int) -> (Int, Int) -> Bool
isValidMove board (srcRow, srcCol) (dstRow, dstCol) =
    let piece = (board !! srcRow) !! srcCol
        rowDiff = dstRow - srcRow
        colDiff = dstCol - srcCol
        absRowDiff = abs rowDiff
        absColDiff = abs colDiff
        destEmpty = (board !! dstRow) !! dstCol == Empty
        forwardMove = case piece of
            Player -> rowDiff == -1
            Bot -> rowDiff == 1
            PlayerKing -> absRowDiff == 1
            BotKing -> absRowDiff == 1
            _ -> False
        diagonal = absRowDiff == 1 && absColDiff == 1
    in destEmpty && forwardMove && diagonal

-- checks if the move is a capture
isValidCapture :: Board -> (Int, Int) -> (Int, Int) -> Bool
isValidCapture board (srcRow, srcCol) (dstRow, dstCol) =
    let piece = (board !! srcRow) !! srcCol
        midRow = (srcRow + dstRow) `div` 2
        midCol = (srcCol + dstCol) `div` 2
        midPiece = (board !! midRow) !! midCol
    in piece /= Empty && midPiece `elem` [Player, Bot] && (board !! dstRow) !! dstCol == Empty

-- sees if someone's won
checkWin :: Board -> Maybe String
checkWin board
    | not (any isPlayerPiece board) = Just "Bot wins!"
    | not (any isBotPiece board) = Just "Player wins!"
    | not (canMove board Player) = Just "Bot wins!"
    | not (canMove board Bot) = Just "Player wins!"
    | otherwise = Nothing
  where
    isPlayerPiece = any (\p -> p == Player || p == PlayerKing)
    isBotPiece = any (\p -> p == Bot || p == BotKing)

-- sees if a piece has any moves left
canMove :: Board -> Piece -> Bool
canMove board pieceType = any pieceHasMoves allPositions
  where
    allPositions = [(r,c) | r <- [0..7], c <- [0..7]]
    pieceHasMoves (r,c) =
        let piece = (board !! r) !! c
        in piece == pieceType &&
           any (\(dr,dc) -> isValidMove board (r,c) (dr,dc) || isValidCapture board (r,c) (dr,dc)) allPositions

-- makes the actual move on the board by adding and removing a piece, which simulates moving a piece; has a check for capture
makeMove :: Board -> (Int, Int) -> (Int, Int) -> Bool -> Board
makeMove board (srcRow, srcCol) (dstRow, dstCol) capture =
    let piece = (board !! srcRow) !! srcCol
        midRow = (srcRow + dstRow) `div` 2
        midCol = (srcCol + dstCol) `div` 2
        boardAfterMove =
            if capture
            then removePiece (removePiece board (srcRow, srcCol)) (midRow, midCol)
            else removePiece board (srcRow, srcCol)
        boardWithPiece = placePiece boardAfterMove (dstRow, dstCol) piece
        -- checks for king promotion
        finalPiece = case piece of
            Player -> if dstRow == 0 then PlayerKing else piece
            Bot -> if dstRow == 7 then BotKing else piece
            _ -> piece
        finalBoard = placePiece (removePiece boardWithPiece (dstRow, dstCol)) (dstRow, dstCol) finalPiece
    in finalBoard

-- removes a piece from the board while maintaining the rest of the board state
removePiece :: Board -> (Int, Int) -> Board
removePiece board (row, col) =
    let (before, target:after) = splitAt row board
        (left, _:right) = splitAt col target
    in before ++ (left ++ Empty : right) : after

-- adds a piece onto the board while maintaining the rest of the board state
placePiece :: Board -> (Int, Int) -> Piece -> Board
placePiece board (row, col) piece =
    let (before, target:after) = splitAt row board
        (left, _:right) = splitAt col target
    in before ++ (left ++ piece : right) : after

-- keeps the game running until incorrect command is put in
gameLoop :: Board -> IO ()
gameLoop board = do
    -- delay to simulate that the move is being done in real time 
    threadDelay $ 1500000 `div` 6
    displayBoard board
    case checkWin board of
        Just winner -> putStrLn winner 
        Nothing -> do
            (srcPos, dstPos) <- getPlayerMove
            -- for capture
            if isValidCapture board srcPos dstPos then do
                putStrLn ""
                let newBoard = makeMove board srcPos dstPos True
                gameLoop newBoard
            -- regular move
            else if isValidMove board srcPos dstPos then do
                putStrLn ""
                let newBoard = makeMove board srcPos dstPos False
                gameLoop newBoard
            else do
                putStrLn "Invalid move, try again."
                putStrLn ""
                gameLoop board

main :: IO ()
main = do
    let board = initialBoard
    gameLoop board