{-# OPTIONS_GHC -XTypeFamilies #-}
{-# OPTIONS_GHC -XFlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Game where

import Parser

--import Control.Applicative
--import Text.Printf
import Data.List

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
    playGame (makeHumanPlayer gomokuMoveParser) (makeHumanPlayer gomokuMoveParser)
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
topLabel n = " " ++ concatMap (\c -> "   " ++ show c) [1..n]

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

--Is there a run of a's of length m within a run of length n with no b's in that area?
-- n <= 0               = (m <= 0) swap this line with the below to count 

nmInARow :: Eq a => Int -> Int -> a -> a -> [[a]] -> Int -> Int -> Int -> Int -> Bool
nmInARow n m a b board x y dx dy
  | m <= 0               = True
  | x < 0 || x >= xSize  = False
  | y < 0 || y >= ySize  = False
  | board !! y !! x == b = False
  | board !! y !! x == a = nmInARow (n - 1) (m - 1) a b board (x + dx) (y + dy) dx dy
  | otherwise            = nmInARow (n - 1) m a b board (x + dx) (y + dy) dx dy
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


