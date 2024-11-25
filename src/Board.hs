module Board where -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:

import Data.Char (chr, isDigit, ord)
import Data.List.Split (splitOn, splitOneOf)

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = Black | White deriving (Show)

data Cell = Piece Player Int | Empty deriving (Show)

data Pos = Pos {col :: Char, row :: Int} deriving (Show)

type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos colA rowA) (Pos colB rowB) = (colA == colB) && (rowA == rowB)

instance Eq Player where
  (==) Black Black = True
  (==) White White = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Piece p1 i1) (Piece p2 i2) = p1 == p2 && i1 == i2
  (==) _ _ = False

-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                  ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################

validateFEN :: String -> Bool
validateFEN fen = (validateCommas (splitOn "/" fen)) && (validateSlashs fen) && (validateFigurs (splitOneOf "/," fen)) && (validateCommasCount fen)

validateCommas :: [[Char]] -> Bool
validateCommas [] = True
validateCommas (x : xs) = (length (filter (== ',') x) == 8) && validateCommas xs

-- maybe I dont need it ???
validateCommasCount :: [Char] -> Bool
validateCommasCount [] = False
validateCommasCount x = (length (filter (== ',') x)) == 72

validateSlashs :: [Char] -> Bool
validateSlashs [] = False
validateSlashs x = (length (filter (== '/') x)) == 8

validateFigurs :: [[Char]] -> Bool
validateFigurs [] = True
validateFigurs (x : xs)
  | (x == "") = True && validateFigurs xs
  | ((head (x) == 'b') || (head (x) == 'w')) && (tail x /= "") && (validateNumber (tail x)) = ((read (tail (x)) < 256) && (read (tail (x)) > 0)) && validateFigurs xs
  | otherwise = False

validateNumber :: [Char] -> Bool
validateNumber [] = True
validateNumber (x : xs) = if (isDigit (x)) && (ord x < 256 && ord x > 0) then validateNumber xs else False

-- isNumber :: Char -> Bool
-- isNumber x = if (x `elem` ['0' .. '9']) then True else False

-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

buildBoard :: String -> Board
buildBoard x =
  if (validateFEN x || x /= "")
    then makeBoard (makeListinList x)
    else []

makeListinList :: String -> [[[Char]]]
makeListinList x = makeList (splitOn "/" x)

makeList :: [[Char]] -> [[String]]
makeList [] = []
makeList (x : xs) = (splitOn "," x) : makeList xs

makeBoard :: [[String]] -> Board
makeBoard [] = []
makeBoard (x : xs) = toBoard x : makeBoard xs

toBoard :: [String] -> [Cell]
toBoard [] = []
toBoard (x : xs) = toCell x : toBoard xs

toCell :: String -> Cell
toCell [] = Empty
toCell x = if (head x) == 'b' then Piece Black (read (tail x)) else Piece White (read (tail x))

-- #############################################################################
-- ####################### line :: Pos -> Pos -> [Pos]  ########################
-- ####################### - 3 Functional Points        ########################
-- ####################### - 1 Coverage Point           ########################
-- #############################################################################

line :: Pos -> Pos -> [Pos]
line (Pos colA rowA) (Pos colB rowB)
  | (ord colA < ord colB && rowA == rowB) = [(Pos colA rowA)] ++ line (Pos (chr ((ord colA) + 1)) rowA) (Pos colB rowB)
  | (ord colA < ord colB && rowA > rowB) = [(Pos colA rowA)] ++ line (Pos (chr ((ord colA) + 1)) (rowA - 1)) (Pos colB rowB)
  | (ord colA < ord colB && rowA < rowB) = [(Pos colA rowA)] ++ line (Pos (chr ((ord colA) + 1)) (rowA + 1)) (Pos colB rowB)
  | (ord colA > ord colB && rowA == rowB) = [(Pos colA rowA)] ++ line (Pos (chr ((ord colA) - 1)) rowA) (Pos colB rowB)
  | (ord colA > ord colB && rowA > rowB) = [(Pos colA rowA)] ++ line (Pos (chr ((ord colA) - 1)) (rowA - 1)) (Pos colB rowB)
  | (ord colA > ord colB && rowA < rowB) = [(Pos colA rowA)] ++ line (Pos (chr ((ord colA) - 1)) (rowA + 1)) (Pos colB rowB)
  | (ord colA == ord colB && rowA > rowB) = [(Pos colA rowA)] ++ line (Pos (chr ((ord colA))) (rowA - 1)) (Pos colB rowB)
  | (ord colA == ord colB && rowA < rowB) = [(Pos colA rowA)] ++ line (Pos (chr ((ord colA))) (rowA + 1)) (Pos colB rowB)
  | otherwise = [(Pos colA rowB)]

-- Done