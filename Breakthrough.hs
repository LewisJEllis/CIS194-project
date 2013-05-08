{-# OPTIONS_GHC -XTypeFamilies #-}
{-# OPTIONS_GHC -XFlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

import Game
import Parser

import Control.Applicative
import Data.List
import Text.Printf

data Breakthrough = Breakthrough
--still work in progress, lots of english in here
instance Game Breakthrough where
  
  data Move Breakthrough = BreakthroughMove Int Int Int
    deriving Eq
  data State Breakthrough = BreakthroughState [[Char]] Bool

  initState = BreakthroughState (replicate 8 "        ") True

  doMove (BreakthroughMove x y d) (BreakthroughState board player)
    = BreakthroughState (replace2D x y ' ' (replace2D tx ty c)) (not player)
      where ty = if player then 1 else (-1)
            tx = d - 2
            c = if player then 'X' else 'O'


  getValidMoves (BreakthroughState board player)
    = [BreakthroughMove x y d | x <- [0..7], y <- [0..7], d <- [1..3], 
      not moving onto own piece, not moving straight at enemy
      board !! y !! x == c]
      where c = if player then 'X' else 'O'????
  
  isDraw state = not (hasWinner state) && length (getValidMoves state) == 0

  hasWinner (BreakthroughState board player)
    = if player then O on top or [X on top or O on bottom]
      where c = if player then 'O' else 'X'

  getWinnerMessage (BreakthroughState _ player)
    = printf "Player %c wins!" (if player then 'O' else 'X')

  showState (BreakthroughState board _)
    = unlines $
        (topLabel 8)
      : surround "  +---+---+---+---+---+---+---+---+"
          [printf "%d%s" i (concat $ surround " | " $ map pure row)
          | (i, row) <- zip nats board
          ]

  showWhichPlayer (BreakthroughState _ player)
    = printf "Player %c's turn." (if player then 'X' else 'O')