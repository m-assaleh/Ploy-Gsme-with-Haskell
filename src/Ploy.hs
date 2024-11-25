module Ploy where -- do NOT CHANGE export of module

import Board
-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:

import Data.Bits (popCount, shift, testBit, (.&.), (.|.))
import Data.Char (chr, ord)

-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, turn :: Int}

instance Show Move where
  show (Move (Pos startarC startR) (Pos tarC tarR) tr) = [startarC] ++ (show startR) ++ "-" ++ [tarC] ++ show tarR ++ "-" ++ show tr

instance Eq Move where
  (==) (Move (Pos startarC1 startR1) (Pos tarC1 tr1) r1) (Move (Pos startarC2 startR2) (Pos tarC2 tr2) r2) =
    startarC1 == startarC2 && startR1 == startR2 && tarC1 == tarC2 && tr1 == tr2 && r1 == r2

rotate :: Int -> Int -> Int
rotate o tr = (.&.) ((.|.) (shift o tr) (shift o (tr - 8))) 255

-- #############################################################################
-- ####################### gameFinished :: Board -> Bool #######################
-- ####################### - 3 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

gameFinished :: Board -> Bool
gameFinished x = (((commanderstartarCheck x True) == True) || ((figurstartarCheck x True True) == True))

figurstartarCheck :: Board -> Bool -> Bool -> Bool
figurstartarCheck _ False False = False
figurstartarCheck (((Piece Black x) : xs) : y) tA tB = if ((x == 170) || (x == 85)) then figurstartarCheck (xs : y) tA tB else figurstartarCheck (xs : y) False tB
figurstartarCheck (((Piece White x) : xs) : y) tA tB = if ((x == 170) || (x == 85)) then figurstartarCheck (xs : y) tA tB else figurstartarCheck (xs : y) tA False
figurstartarCheck (((Empty) : xs) : y) tA tB = figurstartarCheck (xs : y) tA tB
figurstartarCheck ([] : xss) tA tB = figurstartarCheck xss tA tB
figurstartarCheck [] _ _ = True

commanderstartarCheck :: Board -> Bool -> Bool
commanderstartarCheck (((Piece Black x) : xs) : y) b = if ((x == 170) || (x == 85)) && b == True then commanderstartarCheck (xs : y) False else if ((x == 170) || (x == 85)) && b == False then False else commanderstartarCheck (xs : y) b
commanderstartarCheck (((Piece White x) : xs) : y) b = if ((x == 170) || (x == 85)) && b == True then commanderstartarCheck (xs : y) False else if ((x == 170) || (x == 85)) && b == False then False else commanderstartarCheck (xs : y) b
commanderstartarCheck (((Empty) : xs) : y) b = commanderstartarCheck (xs : y) b
commanderstartarCheck (_ : xss) b = commanderstartarCheck xss b
commanderstartarCheck [] _ = True

-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Functional Points                ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################

isValidMove :: Board -> Move -> Bool
isValidMove x (Move (Pos startCol startRow) (Pos targetCol targetRow) trun) = if ((startCol == targetCol) && (startRow == targetRow) && (trun /= 0)) then True else ((((valueOfCell (((prepareBoard x) !! targetRow) !! (valueOfCol targetCol))) == "Empty") || ((valueOfCell (((prepareBoard x) !! targetRow) !! (valueOfCol targetCol))) /= (valueOfCell (((prepareBoard x) !! startRow) !! (valueOfCol startCol))))) && (notOutofBorder startCol startRow targetCol targetRow) && ((valueOfCell (((prepareBoard x) !! startRow) !! (valueOfCol startCol))) /= "Empty") && ((((head (line (Pos startCol startRow) (Pos targetCol targetRow))) == (last (line (Pos startCol startRow) (Pos targetCol targetRow))))) || ((length (line (Pos startCol startRow) (Pos targetCol targetRow)) > 1) && (checkPath (prepareBoard x) (init (tail (line (Pos startCol startRow) (Pos targetCol targetRow))))))) && ((ifShield (((prepareBoard x) !! startRow) !! (valueOfCol startCol))) || (trun == 0)))

-- isValidMove x (Move (Pos startCol startRow) (Pos targetCol targetRow) trun) = (checkIfEmptyCell ((x !! startRow) !! (valueOfCol startCol))) && (checkIfEmptyCell ((x !! targetRow) !! (valueOfCol targetCol)) (valueOfCol targetCol) targetRow) && (notOutofBorder startCol startRow targetCol targetRow)

notOutofBorder :: Char -> Int -> Char -> Int -> Bool
notOutofBorder startCol startRow targetCol targetRow = if (valueOfCol (startCol) /= 0) && (valueOfRow (startRow) /= 0) && (valueOfCol (targetCol) /= 0) && (valueOfRow (targetRow) /= 0) then True else False

checkPath :: Board -> [Pos] -> Bool
checkPath b [] = True
checkPath b ((Pos bCol bRow) : xs) = valueOfCell ((b !! bRow) !! (valueOfCol bCol)) == "Empty"

valueOfCell :: Cell -> String
valueOfCell (Piece White x) = "White"
valueOfCell (Piece Black x) = "Black"
valueOfCell (Empty) = "Empty"

ifShield :: Cell -> Bool
ifShield (Empty) = False
ifShield (Piece _ x) = if 1 == popCount (x) then True else False

valueOfCol :: Char -> Int
valueOfCol x
  | x == 'a' = 1
  | x == 'b' = 2
  | x == 'c' = 3
  | x == 'd' = 4
  | x == 'e' = 5
  | x == 'f' = 6
  | x == 'g' = 7
  | x == 'h' = 8
  | x == 'i' = 9
  | otherwise = 0

valueOfRow :: Int -> Int
valueOfRow x
  | x == 1 = 1
  | x == 2 = 2
  | x == 3 = 3
  | x == 4 = 4
  | x == 5 = 5
  | x == 6 = 6
  | x == 7 = 7
  | x == 8 = 8
  | x == 9 = 9
  | otherwise = 0

prepareBoard :: Board -> Board
prepareBoard [] = []
prepareBoard x = addEmptyRow (addEmptyCol (reverse x))

addEmptyCol :: Board -> Board
addEmptyCol [] = []
addEmptyCol (x : xs) = ([Empty] ++ x) : addEmptyCol xs

addEmptyRow :: Board -> Board
addEmptyRow [] = []
addEmptyRow x = [] : x

-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- ################### - 6 Functional Points                  ##################
-- ################### - 1 Coverage Point                     ##################
-- #############################################################################

possibleMoves :: Pos -> Cell -> [Move]
possibleMoves _ Empty = []
possibleMoves (Pos col row) (Piece color s)
  | (popCount s == 1) = [] ++ testTrn col row 0 s ++ forTest col row 1 0 s ++ testTrn col row 1 s
  | (popCount s == 2) = [] ++ testTrn col row 0 s ++ forTest col row 1 0 s ++ forTest col row 2 0 s
  | (popCount s == 3) = [] ++ testTrn col row 0 s ++ forTest col row 1 0 s ++ forTest col row 2 0 s ++ forTest col row 3 0 s
  | (popCount s == 4) = [] ++ testTrn col row 0 s ++ forTest col row 1 0 s
  | otherwise = []

forTest :: Char -> Int -> Int -> Int -> Int -> [Move]
forTest col row i trn s = test0 col row i trn s ++ test1 col row i trn s ++ test2 col row i trn s ++ test3 col row i trn s ++ test4 col row i trn s ++ test5 col row i trn s ++ test6 col row i trn s ++ test7 col row i trn s

testTrn :: Char -> Int -> Int -> Int -> [Move]
testTrn col row i s
  | (popCount s == 1) = [] ++ forTest col row i 1 s ++ forTest col row i 2 s ++ forTest col row i 3 s ++ forTest col row i 4 s ++ forTest col row i 5 s ++ forTest col row i 6 s ++ forTest col row i 7 s
  | (popCount s == 2) = [] ++ forTest col row i 1 s ++ forTest col row i 2 s ++ forTest col row i 3 s ++ (if (((testBit s 0) && (testBit s 4)) || ((testBit s 1) && (testBit s 5)) || ((testBit s 2) && (testBit s 6) || ((testBit s 3) && (testBit s 7)))) then [] else (forTest col row i 4 s)) ++ forTest col row i 5 s ++ forTest col row i 6 s ++ forTest col row i 7 s
  | (popCount s == 3) = [] ++ forTest col row i 1 s ++ forTest col row i 2 s ++ forTest col row i 3 s ++ forTest col row i 4 s ++ forTest col row i 5 s ++ forTest col row i 6 s ++ forTest col row i 7 s
  | (popCount s == 4) = [] ++ forTest col row i 1 s ++ forTest col row i 3 s ++ forTest col row i 5 s ++ forTest col row i 7 s
  | otherwise = []

test0 :: Char -> Int -> Int -> Int -> Int -> [Move]
test0 col row i trn s = if (testBit s 0) then [] ++ moveN col row i trn else []

test1 :: Char -> Int -> Int -> Int -> Int -> [Move]
test1 col row i trn s = if (testBit s 1) then [] ++ moveNE col row i trn else []

test2 :: Char -> Int -> Int -> Int -> Int -> [Move]
test2 col row i trn s = if (testBit s 2) then [] ++ moveE col row i trn else []

test3 :: Char -> Int -> Int -> Int -> Int -> [Move]
test3 col row i trn s = if (testBit s 3) then [] ++ moveSE col row i trn else []

test4 :: Char -> Int -> Int -> Int -> Int -> [Move]
test4 col row i trn s = if (testBit s 4) then [] ++ moveS col row i trn else []

test5 :: Char -> Int -> Int -> Int -> Int -> [Move]
test5 col row i trn s = if (testBit s 5) then [] ++ moveSW col row i trn else []

test6 :: Char -> Int -> Int -> Int -> Int -> [Move]
test6 col row i trn s = if (testBit s 6) then [] ++ moveW col row i trn else []

test7 :: Char -> Int -> Int -> Int -> Int -> [Move]
test7 col row i trn s = if (testBit s 7) then [] ++ moveNW col row i trn else []

moveN :: Char -> Int -> Int -> Int -> [Move]
moveN col row i trn =
  if ((((col)) `elem` ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']) && ((row + i) `elem` [1, 2, 3, 4, 5, 6, 7, 8, 9]))
    then [] ++ [Move (Pos col row) (Pos col (row + i)) trn]
    else []

moveNE :: Char -> Int -> Int -> Int -> [Move]
moveNE col row i trn =
  if ((((plusChar col (i))) `elem` ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']) && ((row + i) `elem` [1, 2, 3, 4, 5, 6, 7, 8, 9]))
    then [] ++ [Move (Pos col row) (Pos (plusChar col (i)) (row + i)) trn]
    else []

moveE :: Char -> Int -> Int -> Int -> [Move]
moveE col row i trn =
  if ((((plusChar col (i))) `elem` ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']) && ((row) `elem` [1, 2, 3, 4, 5, 6, 7, 8, 9]))
    then [] ++ [Move (Pos col row) (Pos (plusChar col (i)) row) trn]
    else []

moveSE :: Char -> Int -> Int -> Int -> [Move]
moveSE col row i trn =
  if ((((plusChar col (i))) `elem` ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']) && ((row - i) `elem` [1, 2, 3, 4, 5, 6, 7, 8, 9]))
    then [] ++ [Move (Pos col row) (Pos (plusChar col (i)) (row - i)) trn]
    else []

moveS :: Char -> Int -> Int -> Int -> [Move]
moveS col row i trn =
  if ((((col)) `elem` ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']) && ((row - i) `elem` [1, 2, 3, 4, 5, 6, 7, 8, 9]))
    then [] ++ [Move (Pos col row) (Pos col (row - i)) trn]
    else []

moveSW :: Char -> Int -> Int -> Int -> [Move]
moveSW col row i trn =
  if ((((plusChar col (-i))) `elem` ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']) && ((row - i) `elem` [1, 2, 3, 4, 5, 6, 7, 8, 9]))
    then [] ++ [Move (Pos col row) (Pos ((plusChar col (-i))) (row - i)) trn]
    else []

moveW :: Char -> Int -> Int -> Int -> [Move]
moveW col row i trn =
  if ((((plusChar col (-i))) `elem` ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']) && ((row) `elem` [1, 2, 3, 4, 5, 6, 7, 8, 9]))
    then [] ++ [Move (Pos col row) (Pos (plusChar col (-i)) row) trn]
    else []

moveNW :: Char -> Int -> Int -> Int -> [Move]
moveNW col row i trn =
  if ((((plusChar col (-i))) `elem` ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']) && ((row + i) `elem` [1, 2, 3, 4, 5, 6, 7, 8, 9]))
    then [] ++ [Move (Pos col row) (Pos ((plusChar col (-i))) (row + i)) trn]
    else []

plusChar :: Char -> Int -> Char
plusChar x n = chr (ord x + n)

-- #############################################################################
-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Functional Points                            ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################

listMoves :: Board -> Player -> [Move]
listMoves board player
  | gameFinished board == True = []
  | otherwise = dltDup (rightMoves board (qq2 (prepareBoard (board)) player 1 1))

qq2 :: Board -> Player -> Int -> Int -> [Move]
qq2 board player col row
  | row < 10 = qq3 board player col row ++ qq2 board player col (row + 1)
  | otherwise = []

qq3 :: Board -> Player -> Int -> Int -> [Move]
qq3 board player col row
  | col < 10 = qq4 board player col row ++ qq3 board player (col + 1) row
  | otherwise = []

qq4 :: Board -> Player -> Int -> Int -> [Move]
qq4 board player col row =
  if ((board !! row !! col) /= Empty)
    then qq5 player (board !! row !! col) col row
    else []

qq5 :: Player -> Cell -> Int -> Int -> [Move]
qq5 player (Piece color s) col row
  | (color == player) = (possibleMoves (Pos (toChar col) row) (Piece color s)) ++ []
  | otherwise = []

rightMoves :: Board -> [Move] -> [Move]
rightMoves board (x : xs) = if (isValidMove board x) == False then [] ++ (rightMoves board xs) else [x] ++ (rightMoves board xs)
rightMoves _ [] = []

toChar :: Int -> Char
toChar 1 = 'a'
toChar 2 = 'b'
toChar 3 = 'c'
toChar 4 = 'd'
toChar 5 = 'e'
toChar 6 = 'f'
toChar 7 = 'g'
toChar 8 = 'h'
toChar 9 = 'i'

-- https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem
dltDup :: Eq a => [a] -> [a]
dltDup = var []
  where
    var d [] = d
    var d (x : xs)
      | x `elem` d = var d xs
      | otherwise = var (d ++ [x]) xs

-- Done

-- exam :: Board
-- exam = [[Empty,Piece White 170,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]