{-# OPTIONS_GHC -XTypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Game where

-- import AParser

import Data.List
import Text.Printf

{-
Potential games:
Connect 4
Reversi/Othello
Checkers
Gomoku
Go
-}

-------------------- Helper Functions ---------------------

-- Use Data.Sequence instead of lists?

surround :: a -> [a] -> [a]
surround a as = [a] ++ intersperse a as ++ [a]

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

-------------------- Class Definitions --------------------

class Game g where

  data Move  g    :: *
  data State g    :: *

  initState       :: State g
  
--parseMove       :: String -> Maybe (Move g)
  doMove          :: Move g -> State g -> State g
  getValidMoves   :: State g -> [Move g]
  isGameOver      :: State g -> Bool
  gameOverMessage :: State g -> String

  showMove        :: Move g -> String
  showState       :: State g -> String

----------------------- TicTacToe -----------------------

data TicTacToe = TicTacToe

instance Game TicTacToe where
  
  data Move  TicTacToe = TicTacToeMove Int Int
  data State TicTacToe = TicTacToeState [[Char]] Bool

  initState = TicTacToeState ["   ", "   ", "   "] True

  doMove (TicTacToeMove x y) (TicTacToeState board player)
    = TicTacToeState (replace2D x y c board) (not player)
      where c = if player then 'X' else 'O'

  getValidMoves (TicTacToeState board _)
    = [TicTacToeMove x y | x <- [0..2], y <- [0..2], board !! y !! x == ' ']
  
  isGameOver (TicTacToeState board player)
    = or [nInARow 3 c board x y dx dy
          | x <- [0..2], y <- [0..2], dx <- [-1..1], dy <- [-1..1],
            not (dx == 0 && dy == 0)]
      where c = if player then 'O' else 'X'

  gameOverMessage (TicTacToeState _ player)
    = printf "Player %c wins!" (if player then 'O' else 'X')

  showMove (TicTacToeMove x y) = printf "(%d, %d)" x y

  showState (TicTacToeState board player)
    = unlines $
         ["   1 2 3"]
      ++ surround "  +-+-+-+"
         [printf "%d %s" i (surround '|' row) | (i, row) <- zip nats board]
      ++ [printf "Player %c's turn: " (if player then 'X' else 'O')]


{-
class Wrapper g where
  playGame :: g -> Player g -> Player g ->

-}
