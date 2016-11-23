module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku []) = False
isSudoku sudoku = and ((length (rows sudoku) == 9) : map isRow (rows sudoku))

isRow :: [Maybe Int] -> Bool
isRow x = and ((length x == 9) : map isSudokuDigit x)

isSudokuDigit :: Maybe Int -> Bool
isSudokuDigit Nothing = True
isSudokuDigit (Just a) = a <= 9 && a > 0

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sudoku
            | isSudoku sudoku = and (map isRowSolved (rows sudoku))
            | otherwise       = False

isRowSolved :: [Maybe Int] -> Bool
isRowSolved x = and (map isNotNothing x)

isNotNothing :: Maybe Int -> Bool
isNotNothing Nothing = False
isNotNothing _       = True

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = putStr (unlines (map makeString (rows sud)))

makeString :: [Maybe Int] -> String
makeString x = concat (map maybeIntToString x)

maybeIntToString :: Maybe Int -> String
maybeIntToString Nothing = "."
maybeIntToString (Just a) = show a

readSudoku :: FilePath -> IO Sudoku
readSudoku filePath = do
    file <- readFile filePath
    return (Sudoku (map tmp (lines (file))))

tmp :: String -> [Maybe Int]
tmp xs = [stringToMaybeInt y | y <- xs]

stringToMaybeInt :: Char -> Maybe Int
stringToMaybeInt '.'  = Nothing
stringToMaybeInt s    = Just (digitToInt s)

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(9, return Nothing),
                  (1, do
                      x <- elements [1..9];
                        return (Just x))]
--
-- -- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sud = isSudoku sud

-------------------------------------------------------------------------
type Block  = [Maybe Int]
type Row    = [Maybe Int]

--
isOkayBlock :: Block -> Bool
isOkayBlock (x:[]) = True
isOkayBlock (x:xs) = not (contains x xs) && isOkayBlock xs

contains :: Maybe Int -> [Maybe Int] -> Bool
contains Nothing _  = False
contains y (x:[])   = y == x
contains y (x:xs)   = y == x || contains y xs

--
blocks :: Sudoku -> [Block]
blocks sud = groupRows (rows sud)

-- given all rows in a Sudoku splits them in groups of three and creates blocks from those rows
groupRows :: [Row] -> [Block]
groupRows []   = []
groupRows sud  = rowsToBlocks (transpose (take 3 sud)) ++ groupRows (drop 3 sud)

-- Given a list of three rows returns them as a list of the blocks
rowsToBlocks :: [Row] -> [Block]
rowsToBlocks []   = []
rowsToBlocks rows  = (concat (transpose (take 3 rows))):(rowsToBlocks (drop 3 rows))

-- Checks that a given Sudoku only contains valid blocks
isOkay :: Sudoku -> Bool
isOkay sud
        | isSudoku sud = and (map isOkayBlock (blocks sud))
        | otherwise = False
-- --------------------------------------------------------------------------
type Pos = (Int,Int)

blanks :: Sudoku -> [Pos]
blanks sud = helpBlanks2 0 (rows sud)

helpBlanks :: Int -> Int -> [Maybe Int] -> [Pos]
helpBlanks r c ((Just _):[]) = []
helpBlanks r c (Nothing:[]) = ((r,c):[])
helpBlanks r c (Nothing:xs) = ((r,c):helpBlanks r (c+1) xs)
helpBlanks r c ((Just _):xs) = helpBlanks r (c+1) xs

helpBlanks2 :: Int -> [[Maybe Int]] -> [Pos]
helpBlanks2 r (x:[]) = helpBlanks r 0 x
helpBlanks2 r (x:xs) = (helpBlanks r 0 x) ++ (helpBlanks2 (r+1) xs)
