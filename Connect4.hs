{-# OPTIONS_GHC -XTypeFamilies #-}
{-# OPTIONS_GHC -XFlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

import Game
import Parser

import Control.Applicative
import Data.List
import Text.Printf



runValue :: Int -> Int
runValue = (!!) [0,2,6,18,1000]

negamax :: State Connect4 -> Int -> Bool -> Int
negamax s@(Connect4State board _) d p
  | d == 0     = heuristic s
  | otherwise  = (maximum . map (\ss -> (-1)*(negamax ss (d-1) (not p))) . fst . unzip . successors) 
                 (Connect4State board p)

heuristic :: State Connect4 -> Int
heuristic s@(Connect4State _ player) = 
  sum (map (\(c,m) -> (if c == 'O' then 1 else -1) * (runValue m)) (getRuns s))

coeff :: Char -> Bool -> Int
coeff 'O' p = if p then 1 else -1
coeff 'X' p = if p then -1 else 1

getRuns :: State Connect4 -> [(Char, Int)]
getRuns (Connect4State board _) = [(c1, m) | m <- [1..3], (c1,c2) <- [('O','X'),('X','O')], 
  x <- [0..6], y <- [0..5], dx <- [-1..1], dy <- [-1..1],
  (dx /= 0 || dy /= 0) && (nmInARow 4 m c1 c2 board x y dx dy)]

successors :: State Connect4 -> [(State Connect4, Move Connect4)]
successors s = [(doMove m s, m) | m <- (getValidMoves s)]


connect4AI :: Player Connect4
connect4AI s = 
  return (snd (maximumBy (\(h1,_) (h2,_) -> compare h1 h2) tuples))
  where tuples = (map (\(ss,m) -> (negamax ss 2 False, m)) (successors s))


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
            y = length (takeWhile (\r -> (r !! x) == ' ') board) - 1

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
      : surround "  +---+---+---+---+---+---+---+"
          [printf "%d%s" i (concat $ surround " | " $ map pure row)
          | (i, row) <- zip nats board
          ]

  showWhichPlayer (Connect4State _ player)
    = printf "Player %c's turn." (if player then 'X' else 'O')