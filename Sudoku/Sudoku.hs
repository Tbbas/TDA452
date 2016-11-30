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

-- implement prop

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
        | isSudoku sud = and (map isOkayBlock ((blocks sud) ++ (row) ++ (col)))
        | otherwise = False
      where
        row = rows sud
        col = transpose (rows sud)
-- --------------------------------------------------------------------------
type Pos = (Int,Int)

blanks :: Sudoku -> [Pos]
blanks sud = [(rNbr, cNbr) | rNbr <- [0..8], cNbr <- [0..8], isElementNothing (rows sud) rNbr cNbr ]

isElementNothing :: [[Maybe Int]] -> Int -> Int -> Bool
isElementNothing sud rNbr cNbr = ((sud !! rNbr) !! cNbr) == Nothing

prop_blanks = (blanks allBlankSudoku) == [(x,y) | x <- [0..8], y <- [0..8]]
--

(!!=) :: [a] -> (Int, a) -> [a]
(!!=) []      _         = []
(!!=) (x:[])  (pos, el) = el:[]
(!!=) xs      (pos, el)
                        | pos + 1 > length xs = xs
                        | otherwise            = (take (pos) xs) ++ (el : (drop (pos + 1) xs))

prop_replace_length_intacte :: [a] -> (Int, a) -> Bool
prop_replace_length_intacte xs (pos, el)
                  | pos < 0           = prop_replace_length_intacte xs (abs pos, el)
                  | pos > length xs  = prop_replace_length_intacte xs ((length xs), el)
                  | otherwise         = length (xs !!= (pos, el)) == length xs

prop_contains :: Eq a => [a] -> (Int, a) -> Bool
prop_contains [] _            = True
prop_contains xs (pos, el)
                  | pos < 0         = prop_contains xs (abs pos, el)
                  | pos > length xs = prop_contains xs (length xs, el)
                  | otherwise       = elem el (xs !!= (pos, el))

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sud (x,y) el = Sudoku((rows sud) !!= (x,((rows sud) !! x) !!= (y,el)))

prop_update ::Sudoku -> Pos -> Maybe Int -> Property
prop_update sud (x,y) el
                          | x < 0 = prop_update sud (abs x, y) el
                          | y < 0 = prop_update sud (x, abs y) el
                          | x > 8 = prop_update sud (8, y) el
                          | y > 8 = prop_update sud (x, 8) el
                          | otherwise = ((isOkay sud) && (isSudoku sud)) ==> ((rows (update sud (x,y) el) !! x) !! y )== el

-- prop_update_isOkay :: Sudoku -> Pos -> Maybe Int -> Bool
-- prop_update_isOkay sud (x,y) el
--                           | el == Nothing      = prop_update_isOkay sud (x,y) (Just 5)
--                           | (fromJust el) > 9  = prop_update_isOkay sud (x,y) (Just 9)
--                           | (fromJust el) <= 0 = prop_update_isOkay sud (x,y) (Just 1)
--                           | x < 0 = prop_update_isOkay sud (abs x, y) el
--                           | y < 0 = prop_update_isOkay sud (x, abs y) el
--                           | x > 8 = prop_update_isOkay sud (8, y) el
--                           | y > 8 = prop_update_isOkay sud (x, 8) el
--                           | otherwise = isOkay (update sud (x,y) el)

candidates :: Sudoku -> Pos -> [Int]
candidates sud (x,y) = intersect candidatesRow (intersect candidatesCols candidatesBlock)
  where
    candidatesCols   = candidatesInBlock ((transpose (rows sud)) !! y)
    candidatesRow  = candidatesInBlock ((rows sud) !! x)
    candidatesBlock = candidatesInBlock (blocksInSud !! ((y `div` 3) + 3*(x `div` 3)))
    blocksInSud     = blocks sud

candidatesInBlock :: Block -> [Int]
candidatesInBlock block = [x | x <- [1..9], not (contains (Just x) block)]

prop_candidate :: Sudoku -> Pos -> Property
prop_candidate sud (x,y) = ((isOkay sud) && (isSudoku sud) && x > 0 && x < 9 && y > 0 && y < 9)  ==> (isSudoku updatedSud) && (isOkay updatedSud)
  where
    updatedSud = update sud (x,y) (Just (head candidateList))
    candidateList = candidates sud (x,y)
--
solve :: Sudoku -> Maybe Sudoku
solve sud
          | (isSudoku sud) && (isOkay sud) = solve' sud
          | otherwise                      = Nothing


solve' :: Sudoku -> Maybe Sudoku
solve' sud
          | 0 >= length (blanks sud) = Just sud
          | otherwise = solveFor sud pos posValue
          where pos = (head (blanks sud))
                posValue =  candidates sud pos


solveFor :: Sudoku -> Pos -> [Int] -> Maybe Sudoku
solveFor sud pos (c:[])                   = solve (update sud pos (Just c))
solveFor sud pos (c:candidates)           =
      case solve sud of
        Nothing -> solveFor sud pos candidates
        Just sud' -> Just sud'


  --
  --         | sUSud == Nothing              = solveFor sud pos candidates
  --         | otherwise                     = sUSud
  -- where
  --   updatedSud= update sud pos (Just c)
  --   sUSud = solve updatedSud
