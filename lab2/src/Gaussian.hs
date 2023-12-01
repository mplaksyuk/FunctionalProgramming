module Gaussian
  ( Row, Matrix, Vector, Point, minusInf, eps, subRows, multiplyRowNumber, valueToMul, changeValue, customMax, maxIndex, goAlongRows, force, force2, addVectorToMatrix, findMainPivot, CustomAns (..), gaussian, checkResult, checkRowResult, generateMatrix, generateRandom) where

import Data.List (elemIndices)
import System.Random (randoms, newStdGen)

-- Types
type Row = [Float]
type Matrix = [Row]
type Vector = [Float]
type Point = (Int, Int)

-- Constants
minusInf :: Float
minusInf = -100000000000

eps :: Float
eps = 0.00000001

-- Row Operations
subRows :: Row -> Row -> Row
subRows = zipWith (-)

multiplyRowNumber :: Row -> Float -> Row
multiplyRowNumber = map . (*)

valueToMul :: Int -> Row -> Row -> Float
valueToMul ind x y
  | ind < length x && ind < length y && ind >= 0 = x !! ind / y !! ind
  | otherwise = 0

changeValue :: Vector -> Float -> Int -> Vector
changeValue vec newValue ind
  | ind < length vec && ind >= 0 = take ind vec ++ [newValue] ++ drop (ind + 1) vec
  | otherwise = vec

-- Matrix Operations
customMax :: Row -> Float
customMax = foldr max minusInf

maxIndex :: Matrix -> [Int] -> Float -> Int -> Point -> Point
maxIndex [] _ _ _ ind = ind
maxIndex (x:xs) usedRows value rowId ind
  | elem rowId usedRows || value >= customMax x = maxIndex xs usedRows value (rowId + 1) ind
  | value < customMax x && customMax x /= 0 = maxIndex xs usedRows (customMax x) (rowId + 1) (rowId, newInd)
  | otherwise = (-1, -1)
  where
    newInd = head $ elemIndices (customMax x) x

goAlongRows :: Matrix -> Int -> Row -> Point -> Bool -> Matrix
goAlongRows [] _ _ _ _ = []
goAlongRows (mat:mats) rowInd rowToSub maxInd@(maxRowInd, maxColInd) paral
  | paral && maxRowInd == rowInd = nextRowsResult `par` (force divOnOwn `pseq` (divOnOwn : nextRowsResult))
  | paral = nextRowsResult `par` (force subResult `pseq` (subResult : nextRowsResult))
  | maxRowInd == rowInd = divOnOwn : nextRowsResult
  | otherwise = subResult : nextRowsResult
  where
    subResult = if checkLessZero res then multiplyRowNumber res (-1) else res
    divOnOwn = multiplyRowNumber mat (1 / (mat !! maxColInd))
    nextRowsResult = goAlongRows mats (rowInd + 1) rowToSub maxInd paral
    res = subRows mat (multiplyRowNumber rowToSub (valueToMul maxColInd mat rowToSub))

-- Parallel Computation
force :: [a] -> ()
force = foldr seq ()

force2 :: [[a]] -> ()
force2 = mapM_ force

-- Matrix Manipulation
addVectorToMatrix :: Matrix -> Vector -> Matrix
addVectorToMatrix = zipWith (\mat vec -> mat ++ [vec])

findMainPivot :: Matrix -> [Int] -> Bool -> Matrix
findMainPivot mat usedRows paral
  | row == -1 || col == -1 = mat
  | length usedRows /= length mat = findMainPivot (goAlongRows mat 0 rowToSub maxInd paral) (row : usedRows) paral
  | otherwise = mat
  where
    maxInd@(row, col) = maxIndex mat usedRows minusInf 0 (-1, -1)
    rowToSub = mat !! row

-- Result Checking
data CustomAns = Exists Vector | MinusInf | NotExists
  deriving (Show, Eq)

checkRowResult :: Row -> Int
checkRowResult (x:xs)
  | null xs && abs x < eps = -1
  | null xs = 0
  | abs x < eps = checkRowResult xs
  |
