{-# OPTIONS_GHC -XTypeFamilies #-}
{-# OPTIONS_GHC -XFlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Game where

import Parser

import Control.Applicative
import Data.List
import Text.Printf

{-

Todo list
  tictactoe
    --game
    AI
  connect4
    -game
    AI
  gomoku
    game
    AI?
  breakthrough
    game
    AI?
  mnk generalization?
  split into separate files?

Sample run commands:
  Tic-Tac-Toe (move input format: x y):
    playGame (makeHumanPlayer ticTacToeMoveParser) (makeHumanPlayer ticTacToeMoveParser)

    playGame (makeHumanPlayer connect4MoveParser) (makeHumanPlayer connect4MoveParser)
-- forkIO
-- threadDelay
-- http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html
-- http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent-MVar.html

-}

--------------------- Helper Functions ---------------------

-- Use Data.Sequence instead of lists?

surround :: a -> [a] -> [a]
surround a as = [a] ++ intersperse a as ++ [a]

topLabel :: Int -> [Char]
--topLabel n = intercalate "   " $ map show [1..n]
topLabel n = concatMap (\c -> "   " ++ show c) [1..n]

nats :: [Integer]
nats = [1..]

replace :: Int -> a -> [a] -> [a]
replace i a as = (take i as) ++ [a] ++ (drop (i + 1) as)

replace2D :: Int -> Int -> a -> [[a]] -> [[a]]
replace2D x y a as = (replace y (replace x a (as !! y)) as)

nInARow :: Eq a => Int -> a -> [[a]] -> Int -> Int -> Int -> Int -> Bool
nInARow n a board x y dx dy
  | n <= 0               = True
  | x < 0 || x >= xSize  = False
  | y < 0 || y >= ySize  = False
  | board !! y !! x /= a = False
  | otherwise            = nInARow (n - 1) a board (x + dx) (y + dy) dx dy
  where xSize = length (head board)
        ySize = length board



-------------------- Class Definitions ---------------------

-- Is it possible to make Move g an instance of Eq by default?

class Game g where

  data Move  g     :: *
  data State g     :: *

  initState        :: State g
  
  doMove           :: Move g -> State g -> State g
  getValidMoves    :: State g -> [Move g]

  isDraw           :: State g -> Bool
  hasWinner        :: State g -> Bool
  getWinnerMessage :: State g -> String

  showState        :: State g -> String
  showWhichPlayer  :: State g -> String

------------------ General Game Functions ------------------

type Player g = State g -> IO (Move g)

makeHumanPlayer :: (Game g, Eq (Move g)) => Parser (Move g) -> Player g
makeHumanPlayer parser state =
  do putStr "Move: "
     line <- getLine
     case runParser parser line of
       Nothing -> putStrLn "Parse error." >> makeHumanPlayer parser state
       Just (move, _)
         | move `elem` getValidMoves state -> return move
         | otherwise -> putStrLn "Invalid move." >> makeHumanPlayer parser state

playGame :: (Game g) => Player g -> Player g -> IO ()
playGame = playGameFrom initState

playGameFrom :: (Game g) => State g -> Player g -> Player g -> IO ()
playGameFrom state player1 player2 =
  do putStr (showState state)
     if (isDraw state)
       then putStrLn "Draw game."
     else if (hasWinner state)
       then putStrLn (getWinnerMessage state)
     else do
     putStrLn (showWhichPlayer state)
     move <- player1 state
     playGameFrom (doMove move state) player2 player1

-- choose :: (Monad m) => [(Bool, m a)] -> m a
-- (==>) = (,)

-- multiplayer:
--   - rotate through a list of players, or
--   - define a function in Game for who moves next

----------------------- Tic-Tac-Toe ------------------------

data TicTacToe = TicTacToe

ticTacToeMoveParser :: Parser (Move TicTacToe)
ticTacToeMoveParser
  = (\x y -> TicTacToeMove (x - 1) (y - 1)) <$> posInt <*> posInt

instance Game TicTacToe where
  
  data Move TicTacToe = TicTacToeMove Int Int
    deriving Eq
  data State TicTacToe = TicTacToeState [[Char]] Bool

  initState = TicTacToeState ["   ", "   ", "   "] True

  doMove (TicTacToeMove x y) (TicTacToeState board player)
    = TicTacToeState (replace2D x y c board) (not player)
      where c = if player then 'X' else 'O'

  getValidMoves (TicTacToeState board _)
    = [TicTacToeMove x y | x <- [0..2], y <- [0..2], board !! y !! x == ' ']
  
  isDraw state = not (hasWinner state) && length (getValidMoves state) == 0

  hasWinner (TicTacToeState board player)
    = or [nInARow 3 c board x y dx dy
         | x <- [0..2], y <- [0..2], dx <- [-1..1], dy <- [-1..1]
         , not (dx == 0 && dy == 0)
         ]
      where c = if player then 'O' else 'X'

  getWinnerMessage (TicTacToeState _ player)
    = printf "Player %c wins!" (if player then 'O' else 'X')

  showState (TicTacToeState board _)
    = unlines $
        "    1   2   3"
      : surround "  +---+---+---+"
          [printf "%d%s" i (concat $ surround " | " $ map pure row)
          | (i, row) <- zip nats board
          ]
  
  showWhichPlayer (TicTacToeState _ player)
    = printf "Player %c's turn." (if player then 'X' else 'O')


----------------------- Connect4 ------------------------

data Connect4 = Connect4

connect4MoveParser :: Parser (Move Connect4)
connect4MoveParser
  = (\x -> Connect4Move (x - 1)) <$> posInt
--  = (\x -> Connect4Move (x - 1)) <$> posInt <*> posInt
--reduce this to column

instance Game Connect4 where
  
  data Move Connect4 = Connect4Move Int
    deriving Eq
  data State Connect4 = Connect4State [[Char]] Bool

  initState = Connect4State (replicate 6 "       ") True

  doMove (Connect4Move x) (Connect4State board player)
    = Connect4State (replace2D x y c board) (not player)
      where c = if player then 'X' else 'O'
            y = length (takeWhile (== ' ') (board !! x))

  getValidMoves (Connect4State board _)
    = [Connect4Move x | x <- [0..6], board !! 0 !! x == ' '] --each column not full
  
  isDraw state = not (hasWinner state) && length (getValidMoves state) == 0

  hasWinner (Connect4State board player)
    = or [nInARow 4 c board x y dx dy
         | x <- [0..6], y <- [0..5], dx <- [-1..1], dy <- [-1..1]
         , not (dx == 0 && dy == 0)
         ]
      where c = if player then 'O' else 'X'

  getWinnerMessage (Connect4State _ player)
    = printf "Player %c wins!" (if player then 'O' else 'X')

  showState (Connect4State board _)
    = unlines $
        (topLabel 7)
      : ("  +-------+") : 
          [printf "%d%s" i (" |" ++ (concat $ map pure row) ++ "|")
          | (i, row) <- zip nats board
          ]
      ++ ["  +-------+"]
  
  showWhichPlayer (Connect4State _ player)
    = printf "Player %c's turn." (if player then 'X' else 'O')


----------------------- Gomoku ------------------------

data Gomoku = Gomoku

gomokuMoveParser :: Parser (Move Gomoku)
gomokuMoveParser
  = (\x y -> GomokuMove (x - 1) (y - 1)) <$> posInt <*> posInt

instance Game Gomoku where
  
  data Move Gomoku = GomokuMove Int Int
    deriving Eq
  data State Gomoku = GomokuState [[Char]] Bool

  initState = GomokuState (replicate 19 (replicate 19 ' ')) True

  doMove (GomokuMove x y) (GomokuState board player)
    = GomokuState (replace2D x y c board) (not player)
      where c = if player then 'X' else 'O'

  getValidMoves (GomokuState board _)
    = [GomokuMove x y | x <- [0..18], y <- [0..18], board !! y !! x == ' ']
  
  isDraw state = not (hasWinner state) && length (getValidMoves state) == 0

  hasWinner (GomokuState board player)
    = or [nInARow 5 c board x y dx dy
         | x <- [0..18], y <- [0..18], dx <- [-1..1], dy <- [-1..1]
         , not (dx == 0 && dy == 0)
         ]
      where c = if player then 'O' else 'X'

  getWinnerMessage (GomokuState _ player)
    = printf "Player %c wins!" (if player then 'O' else 'X')

  showState (GomokuState board _)
    = unlines $
        (topLabel 7)
      : ("   +-------------------+") : 
          [printf "%02d%s" i (" |" ++ (concat $ map pure row) ++ "|")
          | (i, row) <- zip nats board
          ]
      ++ ["   +-------------------+"]
  
  showWhichPlayer (GomokuState _ player)
    = printf "Player %c's turn." (if player then 'X' else 'O')

