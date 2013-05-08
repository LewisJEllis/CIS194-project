{-# OPTIONS_GHC -XTypeFamilies #-}
{-# OPTIONS_GHC -XFlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

import Game
import Parser

import Control.Applicative
import Data.List
import Text.Printf

data Gomoku = Gomoku

gomokuMoveParser :: Parser (Move Gomoku)
gomokuMoveParser
  = (\x y -> GomokuMove (x - 1) (y - 1)) <$> posInt <*> posInt

runValue :: Int -> Int
runValue = (!!) [0,0,2,10,50,100000]

negamax :: State Gomoku -> Int -> Bool -> Int
negamax s@(GomokuState board _) d p
  | d == 0     = heuristic s
  | otherwise  = (maximum . map (\ss -> (-1)*(negamax ss (d-1) (not p))) . fst . unzip . successors) 
                 (GomokuState board p)

heuristic :: State Gomoku -> Int
heuristic s@(GomokuState _ player) = 
  sum (map (\(c,m) -> (coeff c player) * (runValue m)) (getRuns s))

--Positive or negative based on who's turn
--Related to negamax's idea that h(s,p) = -h(s,p')
coeff :: Char -> Bool -> Int
coeff 'X' p = if p then 1 else -1
coeff 'O' p = if p then -1 else 1
coeff _   _ = 0

--This is the main timesink for the AI, with the 19*19*5 searchspace for runchecking
getRuns :: State Gomoku -> [(Char, Int)]
getRuns (GomokuState board _) = [(c1, m) | m <- [2..5], (c1,c2) <- [('O','X'),('X','O')], 
  x <- [0..18], y <- [0..18], dx <- [-1..1], dy <- [-1..1],
  (dx /= 0 || dy /= 0) && (nmInARow 5 m c1 c2 board x y dx dy)]

successors :: State Gomoku -> [(State Gomoku, Move Gomoku)]
successors s = [(doMove m s, m) | m <- (getValidMoves s)]

gomokuAI :: Player Gomoku
gomokuAI s = 
  return (snd (maximumBy (\(h1,_) (h2,_) -> compare h1 h2) tuples))
  where tuples = (map (\(ss,m) -> (negamax ss 3 False, m)) (successors s))


--Only changes to get here from TicTacToe were dimension numbers
--Interesting topic which generalizes this: 'm,n,k' games.
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
      ("    " ++ (concat $ intersperse " " $ map (printf "%02d") ([1..19] :: [Integer])))
      : ("   +--------------------------------------------------------+") : 
          [printf "%02d%s" i (" | " ++ (concat $ intersperse "  " $ map pure row) ++ "|")
          | (i, row) <- zip nats board
          ]
      ++ ["   +--------------------------------------------------------+"]
  
  showWhichPlayer (GomokuState _ player)
    = printf "Player %c's turn." (if player then 'X' else 'O')
