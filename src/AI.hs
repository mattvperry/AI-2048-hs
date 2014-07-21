module AI (findBestMove) where

import Board
import Control.Applicative
import Data.Ratio
import Data.List.Zipper
import Data.List (maximumBy, transpose, maximum)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)

-- Find best move
findBestMove :: Board -> Moves
findBestMove = fst . maximumBy (comparing snd) . map scoreMove . zip allMoves . repeat
    where scoreMove (m, b) = (m, scoreTopLevel b m)

scoreTopLevel :: Board -> Moves -> Double
scoreTopLevel b m
    | movedBoard == b   = 0.0
    | otherwise         = expectiMiniMax 8 . RandNode 1.0 $ movedBoard
        where movedBoard = moveBoard m b

-- Expectiminimax algorithm
type Prob = Double

data GameTree = PlayNode Prob Board | RandNode Prob Board
    deriving (Show, Eq)

getBoard :: GameTree -> Board
getBoard (PlayNode _ b) = b
getBoard (RandNode _ b) = b

getProb :: GameTree -> Prob
getProb (PlayNode p _) = p
getProb (RandNode p _) = p

fillEmpties :: Int -> [Zipper Int] -> [Board]
fillEmpties x = map (chunksOf 4 . toList . replace x)

emptyPositions :: Zipper Int -> [Zipper Int]
emptyPositions (Zip _ []) = []
emptyPositions z
    | cursor z == 0     = z : empties z
    | otherwise         = empties z
        where empties   = emptyPositions . right

getChildren :: GameTree -> [GameTree]
getChildren (PlayNode p b)  = map (RandNode p) . filter (/=b) $ moveBoard <$> allMoves <*> pure b
getChildren (RandNode p b)  = concat . map placeTile $ [(2, 0.9), (4, 0.1)]
    where placeTile (x, c)  = map (PlayNode ((p / numEmpties (b)) * c)) . fillEmpties x . empties $ b
          numEmpties        = fromIntegral . length . empties
          empties           = emptyPositions . fromList . concat

expectiMiniMax :: Int -> GameTree -> Double
expectiMiniMax x tree
    | getChildren (tree) == []          = 0.0
    | getProb tree < 0.0001 || x == 0   = scoreBoard . getBoard $ tree
expectiMiniMax d n@(PlayNode _ _)   = maximum . map (expectiMiniMax (d-1)) . getChildren $ n
expectiMiniMax d n@(RandNode _ _)   = sum . map (weightScore) . getChildren $ n
    where weightScore n@(PlayNode p _) = p * expectiMiniMax (d-1) n

-- Scoring algorithm
scoreBoard :: Board -> Double
scoreBoard b = fromIntegral $ scoreRows b + scoreCols b + 100000
    where scoreRows = sum . map scoreRow
          scoreCols = scoreRows . transpose

scoreRow :: Row -> Int
scoreRow r = sum . zipWith (*) weights $ heuristics r
    where boolToInt b   =   if b then 1 else 0
          weights       =   [10000, 1000, 20000, 10000, 10000]
          heuristics r  =   [ countOfElem 0 r
                            , closePairs r
                            , boolToInt . maxAtEnd $ r
                            , boolToInt . isOrdered (<) $ r
                            , boolToInt . isOrdered (>) $ r
                            ]

-- Heuristic algorithms
countOfElem :: Eq a => a -> [a] -> Int
countOfElem elem = length . filter (==elem)

isOrdered :: (Int -> Int -> Bool) -> Row -> Bool
isOrdered f = and . pairwise f

closePairs :: Row -> Int
closePairs = countOfElem True . pairwise close
    where close x y
            | x == 0 || y == 0  = False
            | otherwise         = max x y - min x y == min x y

maxAtEnd :: Row -> Bool
maxAtEnd xs
    | maxIndex xs == 0 || maxIndex xs == 3  = True
    | otherwise                             = False
        where maxIndex xs = snd . maximumBy (comparing fst) $ zip xs [0..]

pairwise :: (Int -> Int -> Bool) -> Row -> [Bool]
pairwise f r@(x:xs) = zipWith f r xs
