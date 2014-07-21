module Board
    (
      Moves(..)
    , Board
    , Row
    , moveBoard
    , allMoves
    ) where

import Prelude hiding (Either (..))
import Data.List (transpose)

type Row = [Int]

type Board = [Row]

data Moves = Up | Right | Down | Left
    deriving (Enum, Eq, Bounded)

allMoves :: [Moves]
allMoves = enumFrom minBound

moveBoard :: Moves -> Board -> Board
moveBoard Up      = transpose . map (moveRowLeft) . transpose
moveBoard Right   = map moveRowRight
moveBoard Down    = transpose . map (moveRowRight) . transpose
moveBoard Left    = map moveRowLeft

moveRowLeft :: Row -> Row
moveRowLeft []  = [0, 0, 0, 0]
moveRowLeft [x] = [x, 0, 0, 0]
moveRowLeft r   = take 4 . move $ r
    where move (x:y:xs)
            | x == 0 && y == 0  = moveRowLeft xs
            | x == 0            = moveRowLeft (y:xs)
            | y == 0            = moveRowLeft (x:xs)
            | x == y            = (x + y) : moveRowLeft xs
            | otherwise         = x : moveRowLeft (y:xs)

moveRowRight :: Row -> Row
moveRowRight = reverse . moveRowLeft . reverse
