{-# OPTIONS_GHC -XTypeFamilies #-}
{-# OPTIONS_GHC -XFlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

import Game
import Parser

import Control.Applicative
import Data.List
import Text.Printf

data TicTacToe = TicTacToe

ticTacToeMoveParser :: Parser (Move TicTacToe)
ticTacToeMoveParser
  = (\x y -> TicTacToeMove (x - 1) (y - 1)) <$> posInt <*> posInt


-- AI functions for Tic-Tac-Toe

runValue :: Int -> Int
runValue = (!!) [0, 2, 10, 1000]

negamax :: State TicTacToe -> Int -> Int
negamax s d
  | d <= 0 || isGameOver s = heuristic s
  | otherwise
    = (maximum . map (\s' -> -(negamax s' (d - 1))) . fst . unzip . successors) s

heuristic :: State TicTacToe -> Int
heuristic s@(TicTacToeState _ player) = 
  sum $ map (\(c, m) -> (coeff c player) * (runValue m)) $ getRuns s

coeff :: Char -> Bool -> Int
coeff 'X' p = if p then 1 else -1
coeff 'O' p = if p then -1 else 1
coeff _   _ = 0

getRuns :: State TicTacToe -> [(Char, Int)]
getRuns (TicTacToeState board _) =
  [(c1, m)
  | m <- [1..3], (c1, c2) <- [('O', 'X'), ('X', 'O')], 
    x <- [0..2], y <- [0..2], dx <- [-1..1], dy <- [-1..1],
    not (dx == 0 && dy == 0) && (nmInARow 3 m c1 c2 board x y dx dy)
  ]

successors :: State TicTacToe -> [(State TicTacToe, Move TicTacToe)]
successors s = [(doMove m s, m) | m <- getValidMoves s]

ticTacToeAI :: Player TicTacToe
ticTacToeAI s = 
  return (snd (maximumBy (\(h1, _) (h2, _) -> compare h1 h2) tuples))
  where tuples = map (\(s', m) -> (-(negamax s' 2), m)) $ successors s

-- Game declaration for Tic-Tac-Toe

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
