{-
Gustav Nicander, Fatima Abdullahi, Mattias Samuelsson  
-}

-- | The Tetris game (main module)
module Main where

import ConsoleGUI       -- cabal install ansi-terminal 
--import CodeWorldGUI     -- cabal install codeworld-api
import Shapes

--------------------------------------------------------------------------------
-- * The code that puts all the piece together

main = runGame tetrisGame

tetrisGame = Game { startGame     = startTetris,
                    stepGame      = stepTetris,
                    drawGame      = drawTetris,
                    gameInfo      = defaultGameInfo prop_Tetris,
                    tickDelay     = defaultDelay,
                    gameInvariant = prop_Tetris }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation

-- | The state of the game
data Tetris = Tetris (Vector, Shape) Shape [Shape]
-- The state consists of three parts:
--   * The position and shape of the falling piece
--   * The well (the playing field), where the falling pieces pile up
--   * An infinite supply of random shapes

-- ** Positions and sizes

type Vector = (Int, Int)

-- | The size of the well
wellSize :: (Int, Int)
wellSize   = (wellWidth, wellHeight)
wellWidth  = 10
wellHeight = 20

-- | Starting position for falling pieces
startPosition :: Vector
startPosition = (wellWidth `div` 2 - 1, 0)

-- | Vector addition
vAdd :: Vector -> Vector -> Vector
(x1, y1) `vAdd` (x2, y2) = (x1 + x2, y1 + y2)

-- | Move the falling piece into position
place :: (Vector, Shape) -> Shape
place (v, s) = shiftShape v s

-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (x, s1) s2 xs) =
  prop_Shape s1
    && ((shapeSize s2) == wellSize)
    && not (collision (Tetris (x, s1) s2 xs))

-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls (S s) = S [x ++ r ++ x | r <- topBottomS, x <- [[Just Black]]]
 where
   topBotBlack = [replicate (fst(shapeSize (S s))) (Just Black)]
   topBottomS  = (topBotBlack ++ s ++ topBotBlack)

-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v, p) w _) = addWalls ( combine w (shiftShape v p))

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition, shape1) (emptyShape wellSize) supply
  where
    shape1:supply = [allShapes !! round (r * (fromIntegral(6))) | r <- rs]

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris Tick t = tick t
stepTetris MoveDown t = tick t
stepTetris MoveLeft t = Just (0, movePiece (-1) t)
stepTetris MoveRight t = Just (0, movePiece 1 t)
stepTetris Rotate t = Just (0, rotatePiece t)
stepTetris _ t = Just (0, t) 

rotate :: Tetris -> Tetris
rotate (Tetris (x, s) s1 ls) = Tetris (x, rotateShape s) s1 ls

movePiece :: Int -> Tetris -> Tetris
movePiece a t = moveCol (a, 0) t

--moves the rotated shape to the left if it doesn't fit
adjust :: Tetris -> Tetris
adjust (Tetris ((x, y), s1) w rs)
    | newshapeR > 10 && notoverlaps = rotate $ Tetris ((x - (newshapeR - (newshape s1)), y), s1) w rs
    | otherwise = Tetris ((x, y), s1) w rs
      where 
        newshape s = x + (fst (shapeSize s))
        newshapeR =  newshape (rotateShape s1)
        notoverlaps = not (place ((x, y), rotateShape s1) `overlaps` w)

rotatePiece :: Tetris -> Tetris
rotatePiece t 
  | collision newT  = adjust t
  | otherwise       = newT
  where
    newT = rotate t
    
moveCol :: Vector -> Tetris -> Tetris
moveCol a t
  | collision (newT) = t
  | otherwise        = newT
  where
    newT = move a t

--moves the shape by adding a vector to the shapes current vector
move :: Vector -> Tetris -> Tetris
move a (Tetris (x, s1) w rs ) = Tetris (vAdd a x, s1) w rs

--makes the shape move down every tick
tick :: Tetris -> Maybe (Int, Tetris)
tick t 
  | collision newT   = dropNewPiece t
  | otherwise        = Just (0, newT)
  where
    newT = move (0,1) t
  

collision :: Tetris -> Bool
collision (Tetris ((x, y), s1) w rs) = conditions
  where
    (sx, sy) = shapeSize s1
    conditions = 0 > x 
              || (x + sx) > 10  
              || (y + sy) > 20 
              || w `overlaps` place((x,y), s1)

--dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece (Tetris (v, s1) w (x:xs)) 
  | collision newTetris = Nothing
  |otherwise = Just (score, newTetris) 
  where
    (score, newWell) = clearLines $ w `combine` (place (v, s1))
    newTetris = Tetris (startPosition, x) newWell xs

clearLines :: Shape -> (Int, Shape)
clearLines (S w) = (n, S s)
  where 
    n = length $ filter isComplete w
    s = rows (emptyShape (10, n)) ++ [r | r <- w, not(isComplete r)]
  
--test rows for isComplete
exrad = [Just Black,
         Just Green,
         Just Red, 
         Just Cyan, 
         Just Blue, 
         Just Green, 
         Just Yellow, 
         Just Green, 
         Just Green, 
         Just Green]
         
exrad2 = [Just Black, 
          Just Green, 
          Just Red, 
          Just Cyan, 
          Just Blue, 
          Just Green, 
          Just Yellow, 
          Nothing, 
          Just Green, 
          Just Green]

--checks if row is filled with squares that isn't nothing
isComplete :: Row -> Bool
isComplete []            = True
isComplete (Nothing : _) = False
isComplete (r : rs)      = isComplete rs

rowsOverlap :: Row -> Row -> Bool
rowsOverlap r1 r2 = or $ zipWith (\s1 s2 -> isJust s1 && isJust s2) r1 r2
overlaps :: Shape -> Shape -> Bool
overlaps (S sh1) (S sh2) = or $ zipWith rowsOverlap sh1 sh2




