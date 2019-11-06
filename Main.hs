module Main where

import Data.List (transpose)

-- +-+-+-+
-- |1|2| |
-- +-+-+-+
-- |5|4|3|
-- +-+-+-+
-- |8|6|7|
-- +-+-+-+

-- [ [Just 1,  Just 2, Just 3]
-- , [Nothing, Just 5, Just 4]
-- , [Just 8,  Just 6, Just 7]
-- ]
type Board
  = [[Tile]]

showBoard :: Board -> String
showBoard = undefined

data Move
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight

applyMove :: Board -> Move -> Board
applyMove board move =
  case move of
    MoveLeft -> map (\row -> applyRowMove row MoveRowLeft) board
    MoveRight -> map (\row -> applyRowMove row MoveRowRight) board
    MoveUp -> transpose (applyMove (transpose board) MoveLeft)
    MoveDown -> transpose (applyMove (transpose board) MoveRight)

data MoveRow
  = MoveRowLeft
  | MoveRowRight

applyRowMove :: [Tile] -> MoveRow -> [Tile]
applyRowMove row move =
  case move of
    MoveRowLeft -> moveRowLeft row
    MoveRowRight -> moveRowRight row

moveRowLeft :: [Tile] -> [Tile]
moveRowLeft row =
  case row of
    [] -> []
    Nothing : x : xs -> x : Nothing : xs
    x : ys -> x : moveRowLeft ys

moveRowRight :: [Tile] -> [Tile]
moveRowRight row = reverse (moveRowLeft (reverse row))

-- -- [ ((0,0), 1), .. ]
-- type Board1
--   = [(Coord, Int)]
--
-- type Coord
--   = (Int, Int)
--
-- type Board2
--   = [Tile]
--
type Tile
  = Maybe Int
--
-- type Board3
--   = Coord -> Maybe Int

main :: IO ()
main = do
  pure ()
