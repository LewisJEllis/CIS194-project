{-# OPTIONS_GHC -XTypeFamilies #-}
{-# OPTIONS_GHC -XFlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Game where

import Parser

import Data.List


--------------------- Helper Functions ---------------------

-- Intersperses an element throughout a list, and appends it to each end as well
surround :: a -> [a] -> [a]
surround a as = [a] ++ intersperse a as ++ [a]

topLabel :: Int -> [Char]
topLabel n = " " ++ concatMap (\c -> "   " ++ show c) [1..n]

-- List of natural numbers
nats :: [Integer]
nats = [1..]

-- Replaces an element in a list at the given index
replace :: Int -> a -> [a] -> [a]
replace i a as = (take i as) ++ [a] ++ (drop (i + 1) as)

-- Replaces an element in a 2D board at the given coordinates
replace2D :: Int -> Int -> a -> [[a]] -> [[a]]
replace2D x y a as = (replace y (replace x a (as !! y)) as)

-- Searches for n of the given element in a row in the provided board
nInARow :: Eq a => Int -> a -> [[a]] -> Int -> Int -> Int -> Int -> Bool
nInARow n a board x y dx dy
  | n <= 0               = True
  | x < 0 || x >= xSize  = False
  | y < 0 || y >= ySize  = False
  | board !! y !! x /= a = False
  | otherwise            = nInARow (n - 1) a board (x + dx) (y + dy) dx dy
  where xSize = length (head board)
        ySize = length board

--Is there a run of a's of length m within a run of length n with no b's in the way?
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

---------------- Class and Type Definitions ----------------

-- General Game class definition
class Game g where

  data Move  g     :: *
  data State g     :: *

  initState        :: State g
  
  doMove           :: Move g -> State g -> State g
  getValidMoves    :: State g -> [Move g]

  isDraw           :: State g -> Bool
  hasWinner        :: State g -> Bool
  getWinnerMessage :: State g -> String
  
  isGameOver       :: State g -> Bool
  isGameOver s = isDraw s || hasWinner s

  showState        :: State g -> String
  showWhichPlayer  :: State g -> String

-- The Player type used to play games
type Player g = State g -> IO (Move g)

------------------ General Game Functions ------------------

-- Takes as input a move parser, and returns a human player
makeHumanPlayer :: (Game g, Eq (Move g)) => Parser (Move g) -> Player g
makeHumanPlayer parser state =
  do putStr "Move: "
     line <- getLine
     case runParser parser line of
       Nothing -> putStrLn "Parse error." >> makeHumanPlayer parser state
       Just (move, _)
         | move `elem` getValidMoves state -> return move
         | otherwise -> putStrLn "Invalid move." >> makeHumanPlayer parser state

-- Plays a game with the two given players, starting from the initial state
playGame :: (Game g) => Player g -> Player g -> IO ()
playGame = playGameFrom initState

-- Plays a game with the two given players, starting from the given state
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

