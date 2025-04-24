module Main where

-- ShogiMogi. Shogi engine in Haskell
-- Copyright (C) 2025 Toni Helminen
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

import Data.List (find, findIndices, intersperse)
import Data.Char (toUpper)
import System.Random (randomRIO, newStdGen)
import Control.Monad (when)
import System.IO (hFlush, stdout)

-- Types
type Position = (Int, Int)  -- (row, column), 1-based indexing
type BoardSize = (Int, Int)  -- (rows, columns)

data PieceType = King | Rook | Bishop | Gold | Silver | Knight | Lance | Pawn
               | PromotedRook | PromotedBishop | PromotedSilver | PromotedKnight | PromotedLance | PromotedPawn
    deriving (Eq, Show, Enum, Bounded)

data Piece = Piece { pieceType :: PieceType, owner :: Player }
    deriving (Eq)

data Player = Black | White
    deriving (Eq, Show)

data GameState = GameState
    { board :: [[Maybe Piece]]
    , currentPlayer :: Player
    , capturedPieces :: [(Player, PieceType)]
    , gameOver :: Bool
    }

instance Show Piece where
    show (Piece pt player) =
        case player of
            Black -> case pt of
                King -> "玉"
                Rook -> "飛"
                Bishop -> "角"
                Gold -> "金"
                Silver -> "銀"
                Knight -> "桂"
                Lance -> "香"
                Pawn -> "歩"
                PromotedRook -> "龍"
                PromotedBishop -> "馬"
                PromotedSilver -> "全"
                PromotedKnight -> "圭"
                PromotedLance -> "杏"
                PromotedPawn -> "と"
            White -> case pt of
                King -> "王"
                Rook -> "飛"
                Bishop -> "角"
                Gold -> "金"
                Silver -> "銀"
                Knight -> "桂"
                Lance -> "香"
                Pawn -> "歩"
                PromotedRook -> "龍"
                PromotedBishop -> "馬"
                PromotedSilver -> "全"
                PromotedKnight -> "圭"
                PromotedLance -> "杏"
                PromotedPawn -> "と"

-- Constants
shogiBoardSize :: BoardSize
shogiBoardSize = (9, 9)

initialBoard :: [[Maybe Piece]]
initialBoard =
    [ [Just (Piece Lance Black), Just (Piece Knight Black), Just (Piece Silver Black), Just (Piece Gold Black), Just (Piece King Black), Just (Piece Gold Black), Just (Piece Silver Black), Just (Piece Knight Black), Just (Piece Lance Black)]
    , [Nothing, Just (Piece Rook Black), Nothing, Nothing, Nothing, Nothing, Nothing, Just (Piece Bishop Black), Nothing]
    , [Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black)]
    , replicate 9 Nothing
    , replicate 9 Nothing
    , replicate 9 Nothing
    , replicate 9 Nothing
    , [Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White)]
    , [Nothing, Just (Piece Bishop White), Nothing, Nothing, Nothing, Nothing, Nothing, Just (Piece Rook White), Nothing]
    , [Just (Piece Lance White), Just (Piece Knight White), Just (Piece Silver White), Just (Piece Gold White), Just (Piece King White), Just (Piece Gold White), Just (Piece Silver White), Just (Piece Knight White), Just (Piece Lance White)]
    ]

-- Game initialization
newGame :: IO GameState
newGame = do
    _ <- newStdGen  -- Initialize random generator
    return $ GameState
        { board = initialBoard
        , currentPlayer = Black
        , capturedPieces = []
        , gameOver = False
        }

-- Game logic
isValidPosition :: Position -> Bool
isValidPosition (row, col) =
    row >= 1 && row <= fst shogiBoardSize && col >= 1 && col <= snd shogiBoardSize

getPiece :: GameState -> Position -> Maybe Piece
getPiece gs (row, col) =
    if isValidPosition (row, col)
    then board gs !! (row - 1) !! (col - 1)
    else Nothing

setPiece :: GameState -> Position -> Maybe Piece -> GameState
setPiece gs (row, col) piece =
    if isValidPosition (row, col)
    then gs { board = newBoard }
    else gs
  where
    newBoard = take (row-1) (board gs) ++
               [take (col-1) (board gs !! (row-1)) ++ [piece] ++ drop col (board gs !! (row-1))] ++
               drop row (board gs)

-- Move validation
canPromote :: PieceType -> Bool
canPromote pt = case pt of
    King -> False
    Gold -> False
    PromotedRook -> False
    PromotedBishop -> False
    PromotedSilver -> False
    PromotedKnight -> False
    PromotedLance -> False
    PromotedPawn -> False
    _ -> True

isPromoted :: PieceType -> Bool
isPromoted pt = case pt of
    PromotedRook -> True
    PromotedBishop -> True
    PromotedSilver -> True
    PromotedKnight -> True
    PromotedLance -> True
    PromotedPawn -> True
    _ -> False

promotePiece :: PieceType -> PieceType
promotePiece pt = case pt of
    Rook -> PromotedRook
    Bishop -> PromotedBishop
    Silver -> PromotedSilver
    Knight -> PromotedKnight
    Lance -> PromotedLance
    Pawn -> PromotedPawn
    _ -> pt

unpromotePiece :: PieceType -> PieceType
unpromotePiece pt = case pt of
    PromotedRook -> Rook
    PromotedBishop -> Bishop
    PromotedSilver -> Silver
    PromotedKnight -> Knight
    PromotedLance -> Lance
    PromotedPawn -> Pawn
    _ -> pt

-- Movement rules
getMovementVectors :: Piece -> Position -> [(Int, Int)]
getMovementVectors (Piece pt player) (row, col) =
    case pt of
        King -> kingMoves
        Rook -> rookMoves
        Bishop -> bishopMoves
        Gold -> goldMoves player
        Silver -> silverMoves player
        Knight -> knightMoves player
        Lance -> lanceMoves player
        Pawn -> pawnMoves player
        PromotedRook -> promotedRookMoves
        PromotedBishop -> promotedBishopMoves
        PromotedSilver -> goldMoves player
        PromotedKnight -> goldMoves player
        PromotedLance -> goldMoves player
        PromotedPawn -> goldMoves player
  where
    kingMoves = [(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1),(0,-1),(1,-1)]
    rookMoves = concatMap (\d -> [(d,0),(0,d),(-d,0),(0,-d)]) [1..8]
    bishopMoves = concatMap (\d -> [(d,d),(d,-d),(-d,d),(-d,-d)]) [1..8]
    goldMoves Black = [(1,0),(1,1),(0,1),(0,-1),(-1,0),(1,-1)]
    goldMoves White = [(-1,0),(-1,1),(0,1),(0,-1),(1,0),(-1,-1)]
    silverMoves Black = [(1,0),(1,1),(1,-1),(-1,1),(-1,-1)]
    silverMoves White = [(-1,0),(-1,1),(-1,-1),(1,1),(1,-1)]
    knightMoves Black = [(2,1),(2,-1)]
    knightMoves White = [(-2,1),(-2,-1)]
    lanceMoves Black = [(d,0) | d <- [1..8]]
    lanceMoves White = [(-d,0) | d <- [1..8]]
    pawnMoves Black = [(1,0)]
    pawnMoves White = [(-1,0)]
    promotedRookMoves = kingMoves ++ rookMoves
    promotedBishopMoves = kingMoves ++ bishopMoves

getValidMoves :: GameState -> Position -> [Position]
getValidMoves gs pos =
    case getPiece gs pos of
        Nothing -> []
        Just piece@(Piece _ owner) ->
            if owner /= currentPlayer gs
            then []
            else filter (isValidMove gs pos) possibleMoves
  where
    possibleMoves = map (\(dr,dc) -> (fst pos + dr, snd pos + dc))
                   (getMovementVectors (fromJust (getPiece gs pos)) pos)
    fromJust (Just x) = x

isValidMove :: GameState -> Position -> Position -> Bool
isValidMove gs from to =
    isValidPosition to &&
    case getPiece gs to of
        Nothing -> True
        Just (Piece _ owner) -> owner /= currentPlayer gs

-- Game execution
makeMove :: GameState -> Position -> Position -> Bool -> IO GameState
makeMove gs from to promote = do
    case getPiece gs from of
        Nothing -> return gs
        Just piece@(Piece pt owner) -> do
            -- Check if move is valid
            if not (isValidMove gs from to)
            then do
                putStrLn "Invalid move!"
                return gs
            else do
                -- Capture piece if needed
                let captured = getPiece gs to
                let newCaptured = case captured of
                        Nothing -> capturedPieces gs
                        Just (Piece pt' _) -> (owner, unpromotePiece pt') : capturedPieces gs

                -- Move the piece
                let movedPiece = if promote && canPromote pt
                                then Just (Piece (promotePiece pt) owner)
                                else Just piece
                let gs' = setPiece (setPiece gs from Nothing) to movedPiece

                -- Switch player
                return gs'
                    { currentPlayer = if owner == Black then White else Black
                    , capturedPieces = newCaptured
                    }

-- Random AI moves
getAllPossibleMoves :: GameState -> [(Position, Position)]
getAllPossibleMoves gs = do
    let player = currentPlayer gs
    row <- [1..fst shogiBoardSize]
    col <- [1..snd shogiBoardSize]
    let pos = (row, col)
    case getPiece gs pos of
        Just (Piece _ owner) | owner == player ->
            map (\to -> (pos, to)) (getValidMoves gs pos)
        _ -> []

makeRandomMove :: GameState -> IO GameState
makeRandomMove gs = do
    let possibleMoves = getAllPossibleMoves gs
    if null possibleMoves
    then do
        putStrLn $ show (currentPlayer gs) ++ " has no valid moves!"
        return gs { gameOver = True }
    else do
        randomIndex <- randomRIO (0, length possibleMoves - 1)
        let (from, to) = possibleMoves !! randomIndex

        -- Randomly decide to promote (50% chance if possible)
        let piece = fromJust (getPiece gs from)
        let shouldPromote = canPromote (pieceType piece)
        promote <- if shouldPromote then randomRIO (False, True) else return False

        putStrLn $ show (currentPlayer gs) ++ " moves from " ++ positionToNotation from ++
                  " to " ++ positionToNotation to ++ if promote then " (promoted)" else ""

        newGs <- makeMove gs from to promote
        return newGs

-- UI functions
printBoard :: GameState -> IO ()
printBoard gs = do
    let (rows, cols) = shogiBoardSize
    putStr "   "
    mapM_ (\c -> putStr $ [toEnum (fromEnum '１' + c - 1)] ++ " ") [1..cols]
    putStrLn ""
    mapM_ printRow [1..rows]
  where
    printRow row = do
        putStr $ show row ++ " "
        mapM_ (printCell row) [1..snd shogiBoardSize]
        putStrLn ""
    printCell row col = do
        case getPiece gs (row, col) of
            Nothing -> putStr "・"
            Just piece -> putStr $ show piece

positionToNotation :: Position -> String
positionToNotation (row, col) =
    [toEnum (fromEnum '１' + col - 1)] ++ show row

notationToPosition :: String -> Maybe Position
notationToPosition [col, row] =
    let colNum = fromEnum col - fromEnum '１' + 1
        rowNum = read [row] :: Int
    in if colNum >= 1 && colNum <= snd shogiBoardSize &&
          rowNum >= 1 && rowNum <= fst shogiBoardSize
       then Just (rowNum, colNum)
       else Nothing
notationToPosition _ = Nothing

-- Main game loop
gameLoop :: GameState -> IO ()
gameLoop gs = do
    printBoard gs
    putStrLn $ "\nCurrent player: " ++ show (currentPlayer gs)
    putStrLn "Captured pieces:"
    putStrLn $ "Black: " ++ show (filter (\(p,_) -> p == Black) (capturedPieces gs))
    putStrLn $ "White: " ++ show (filter (\(p,_) -> p == White) (capturedPieces gs))

    if gameOver gs
    then putStrLn "Game over!"
    else case currentPlayer gs of
        Black -> do
            putStrLn "Black (AI) is thinking..."
            newGs <- makeRandomMove gs
            gameLoop newGs
        White -> do
            putStrLn "Enter move as 'from to' (e.g., '７六 ７五') or 'resign':"
            hFlush stdout
            input <- getLine
            case input of
                "resign" -> putStrLn $ show (currentPlayer gs) ++ " resigns. Game over!"
                _ -> case words input of
                    [fromStr, toStr] -> do
                        case (notationToPosition fromStr, notationToPosition toStr) of
                            (Just from, Just to) -> do
                                -- Check if promotion is possible
                                let piece = fromJust (getPiece gs from)
                                let promote = canPromote (pieceType piece)
                                let askPromote = if promote
                                                 then do
                                                     putStrLn "Promote? (y/n):"
                                                     hFlush stdout
                                                     answer <- getLine
                                                     return (answer == "y")
                                                 else return False
                                doPromote <- askPromote
                                newGs <- makeMove gs from to doPromote
                                gameLoop newGs
                            _ -> do
                                putStrLn "Invalid positions!"
                                gameLoop gs
                    _ -> do
                        putStrLn "Invalid input!"
                        gameLoop gs

-- Main function
main :: IO ()
main = do
    putStrLn "Welcome to Shogi with Random AI!"
    putStrLn "You are playing as White against the random Black AI"
    putStrLn "Black moves first, then White"
    putStrLn "Enter moves in Japanese notation (e.g., '７六 ７五')"
    putStrLn "Type 'resign' to give up\n"

    gs <- newGame
    gameLoop gs
