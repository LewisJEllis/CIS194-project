{-# OPTIONS_GHC -XTypeFamilies #-}
{-# OPTIONS_GHC -XFlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

import Game
import Parser

import Control.Applicative
import Text.Printf

data Breakthrough = Breakthrough

--Chess variant. Only pawns. Normal pawn movement, except they can always move diagonally forward.
--Get a pawn to the opposite side to win.

--Figure out the destination of a piece when applying a move
dest :: Int -> Int -> Int -> Bool -> (Int, Int)
dest x y d p = ((x + d - 2), y - (if p then 1 else (-1)))


--d is an integer 1-3 representing diagonal left, straight, and diagonal right
breakthroughMoveParser :: Parser (Move Breakthrough)
breakthroughMoveParser
  = (\x y d -> BreakthroughMove (x - 1) (y - 1) d) <$> posInt <*> posInt <*> posInt


instance Game Breakthrough where  
  data Move Breakthrough = BreakthroughMove Int Int Int
    deriving Eq
  data State Breakthrough = BreakthroughState [[Char]] Bool

  initState = BreakthroughState 
              (["OOOOOOOO","OOOOOOOO"] ++ (replicate 4 "        ") ++ ["XXXXXXXX","XXXXXXXX"])
              True

  doMove (BreakthroughMove x y d) (BreakthroughState board player)
    = BreakthroughState (replace2D x y ' ' (replace2D dx dy c board)) (not player)
      where (dx, dy) = dest x y d player
            c = if player then 'X' else 'O'

  --Not going off the board, and you have a piece there to start
  --Not running over your own piece, and not going straight into an opponent
  getValidMoves (BreakthroughState board player)
    = [BreakthroughMove x y d | x <- [0..7], y <- [0..7], d <- [1..3], 
      let (dx,dy) = dest x y d player,
      (dx >= 0 && dx < 8 && dy >= 0 && dy < 8 
      && (board !! y !! x == c)
      && not (board !! dy !! dx == c) 
      && not (d == 2 && board !! dy !! dx == e))]
      where c = if player then 'X' else 'O'
            e = if player then 'O' else 'X'
  
  isDraw state = not (hasWinner state) && length (getValidMoves state) == 0

  --Wincondition: Xs in the top row or Os in the bottom row
  hasWinner (BreakthroughState board player)
    | player    = elem 'O' (board !! 7)
    | otherwise = elem 'X' (board !! 0)

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
