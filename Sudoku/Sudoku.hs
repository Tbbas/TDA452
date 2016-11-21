module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List.Split

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
type Block = [Maybe Int]

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
blocks sud = undefined

tmp x:xs = undefined

splitRow :: [Maybe Int] -> [[Maybe Int]]
splitRow xs = chunksOf 3 xs
--------------------------------------------------------------------------


example :: Sudoku
example =
  Sudoku
    [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
    , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]
