
-- | Types and functions for shapes. The list of all tetris pieces.

module Shapes where

import Data.List (transpose)
import Data.Maybe (isNothing)
import Test.QuickCheck

-- * Shapes

type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
  deriving (Eq, Bounded, Enum, Show)

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.

data Shape = S [Row] deriving (Eq)
type Row   = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs

-- * Showing shapes

showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]
    
    showSquare Nothing      = '.'
    showSquare (Just Black) = '#' -- can change to '█' on linux/mac
    showSquare (Just Grey)  = 'g' -- can change to '▓'
    showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss) ++ r

-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of 4 connected blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes] 
   where
      makeSquares = map (map colour)
      colour c    = lookup c [ ('I', Red), ('J', Grey), ('T', Blue)
                             , ('O', Yellow), ('Z',Cyan), ('L', Green)
                             , ('S', Purple) ]
      shapes = 
              [["I",
                "I",
                "I",
                "I"],
              [" J",
               " J",
               "JJ"],
              [" T",
               "TT",
               " T"],
              ["OO",
               "OO"],
              [" Z",
               "ZZ",
               "Z "],
              ["LL",
               " L",
               " L"],
              ["S ",
               "SS",
               " S"]]

-- * Some simple functions

-- ** A1

{-Creates an empty shape consisting of s many empty rows
consisting of r many empty blocks-}
emptyShape :: (Int, Int) -> Shape
emptyShape (r, s) = S (replicate s (emptyrow r))

--Creates an empty row of r length
emptyrow :: Int -> Row
emptyrow r = replicate r Nothing

-- ** A2

-- | The size (width and height) of a shape

{-takes the length of rows and the amounts of blocks in a row
by taking the length of the whole shape divided by the rows-}
shapeSize :: Shape -> (Int, Int)
shapeSize (S s) = ( length (concat s) `div` length s, length s)

-- ** A3

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount (S s) = blockCount' (concat s)

--Recursively counts every block that isn't Nothing
blockCount' :: Row -> Int
blockCount' [] = 0
blockCount' [Nothing] = 0
blockCount' [x] = 1
blockCount' (Nothing : xs) = blockCount' xs
blockCount' (x : xs) = 1 + blockCount' xs

-- * The Shape invariant

-- ** A4

-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)

-- Checks all properties together

prop_Shape :: Shape -> Bool
prop_Shape (S rs) = and [h > 0, w > 0, rectangular]
    where (w,h)       = shapeSize (S rs)
          rectangular = and [length r == w | r <- rs]
  
--checks that all rows contains the same amount of blocks
prop_Equal :: Shape -> Bool
prop_Equal (S []) = True
prop_Equal (S [x]) = True
prop_Equal (S (x : y : xs)) = length x == length y && prop_Equal (S (y : xs))

-- * Test data generators

-- ** A5

-- | A random generator for colours
genColour :: Gen Colour
genColour =
  elements
    [ Black,
      Red,
      Green,
      Yellow,
      Blue,
      Purple,
      Cyan,
      Grey
    ]

instance Arbitrary Colour where
  arbitrary = genColour

-- ** A6

-- | A random generator for shapes
genShape :: Gen Shape
genShape = elements allShapes

instance Arbitrary Shape where
  arbitrary = genShape

-- * Transforming shapes

-- ** A7

--example shape for testing
it :: Shape
it = allShapes !! 1

it2 :: Shape
it2 = allShapes !! 2

it3 :: Shape
it3 = S (zipWith (++) (rows (emptyShape (4, 4))) (rows (allShapes !! 0)))

-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape (S r) = S (reverse (transpose r))

-- ** A8

-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int, Int) -> Shape -> Shape
shiftShape (i, n) (S xs) = shiftPadShape shiftVert shiftR
  where
    shiftVert = shiftPadVertical i n xs
    shiftR = shiftPadRight i [] xs

--helper function to shiftShape and padShape
shiftPadShape :: [Row] -> [Row] -> Shape
shiftPadShape f g = S (f ++ g)

--helper function for shifting and padding vertical
shiftPadVertical :: Int -> Int -> [Row] -> [Row]
shiftPadVertical i n xs = rows (emptyShape (i + (length (transpose xs)), n))

--helper function for shifting and padding right
-- shifts [row] to the left or right by adding emptyrows from the right or left resp
shiftPadRight :: Int -> [Row] -> [Row] -> [Row]
-- shifts [row] to the left if 2nd [Row] is empty
shiftPadRight i xs [] = map (\x -> x ++ emptyrow i) xs
-- shifts [row] to the right if 1st [Row] is empty
shiftPadRight i [] hs = map (\x -> emptyrow i ++ x) hs
-- takes two [Row] and combines each list in both [Row] with emptyrows in between
shiftPadRight i xs hs = [x ++ emptyrow i ++ h | h <- hs, x <- xs]

-- ** A9

-- | padShape adds empty square below and to the right of the shape
padShape :: (Int, Int) -> Shape -> Shape
padShape (i, n) xs = rotate180 (shiftShape (i,n) (rotate180 xs))
  where rotate180 sh = rotateShape $ rotateShape sh

-- ** A10

-- | pad a shape to a given size
-- m = lägger till element i varje list, n lägger till rad 
padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (m, n) (S r) = padShape (m - length (head r), n - length r) (S r)

-- * Comparing and combining shapes

-- ** B1

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
(S (x : xs)) `overlaps` (S (y : ys))
  | xs == [] = rowsoverlap x y
  | ys == [] = rowsoverlap x y
  | otherwise = rowsoverlap x y || overlaps (S xs) (S ys)

-- | checks if two rows overlap
rowsoverlap :: Row -> Row -> Bool
rowsoverlap [] _ = False
rowsoverlap _ [] = False
rowsoverlap (x : xs) (y : ys)
  | x /= Nothing && y /= Nothing = True
  | otherwise = rowsoverlap xs ys

-- ** B2

-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square -> Square -> Square) -> Shape -> Shape -> Shape
zipShapeWith f (S s1) (S s2) = S (zipWith (zipWith f) s1 s2)

blackClashes :: Shape -> Shape -> Shape
blackClashes s1 s2 = zipShapeWith clash s1 s2
  where
    clash :: Square -> Square -> Square
    clash Nothing Nothing = Nothing
    clash Nothing s = s
    clash s Nothing = s
    clash (Just c1) (Just c2) = Just Black

-- ** B3

-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.

combine :: Shape -> Shape -> Shape
s1 `combine` s2
  | s1 `overlaps` s2 = error "Combine: Clashing shapes"
  | otherwise = blackClashes (padshape' s2) (padshape' s1)
  where
    padshape' = padShapeTo (r, c)
    (r, c) = (max (fst (shapeSize s1)) (fst (shapeSize s2)),
             (max (snd (shapeSize s1)) (snd (shapeSize s2))))
