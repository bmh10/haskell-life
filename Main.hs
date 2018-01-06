module Main where

import Graphics.Gloss
import qualified Graphics.Gloss.Game as GG
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.IO  
import Control.Monad
import Data.Fixed
import Data.List
import Data.Maybe

fps = 5
numLevels = 4
pointsPerLevel = 10
width = 420 -- 28 * 15
height = 465 + dashboardHeight -- 31 * 15
dashboardHeight = 20
offset = 100
tileSize = 15
maxTileHoriz = 27
window = InWindow "Life" (width, height) (offset, offset)
background = black

data LifeGame = Game
  { 
    level :: [String],
    seconds :: Float,            -- Game timer
    paused :: Bool              -- Paused or not
  } deriving Show 

-- Tile functions
getTile :: Int -> Int -> LifeGame -> Char
getTile x y g = (level g) !! y !! x

setTile :: Int -> Int -> Char -> LifeGame -> LifeGame
setTile x y c g = g { level = updatedLevel}
  where updatedLevel = setAtIdx y (setAtIdx x c ((level g) !! y)) (level g)

onTick :: LifeGame -> Bool -> Int -> a -> a -> a 
onTick g c t a b = if (c && (mod (round (seconds g)) t) == 0) then a else b

-- Map tile coords ((0,0) is top-left tile) to actual screen coords ((0, 0) is center of screen)
tileToCoord :: (Int, Int) -> (Float, Float) 
tileToCoord (x, y) = (fromIntegral x*tileSize + tileSize/2 - fromIntegral width/2, fromIntegral height/2 - fromIntegral y*tileSize - tileSize/2)

setAtIdx :: Int -> a -> [a] -> [a]
setAtIdx idx val xs = take idx xs ++ [val] ++ drop (idx+1) xs

-- Rendering
render :: LifeGame -> Picture 
render g = pictures [renderLevel g, 
                     renderDashboard g]

renderDashboard :: LifeGame -> Picture
renderDashboard g = scorePic
  where
    scorePic = color white $ translate (-30) (-fromIntegral height/2 + 5) $ scale 0.1 0.1 $ text $ "Dashboard"

renderLevel :: LifeGame -> Picture
renderLevel game = renderLines (level game) 0

renderLines :: [String] -> Int -> Picture
renderLines [] _ = blank
renderLines (l:ls) y = pictures [renderLine l 0 y, renderLines ls (y+1)]

renderLine :: String -> Int -> Int -> Picture
renderLine [] _ _      = blank
renderLine (t:ts) x y  = pictures [renderTile t x y, renderLine ts (x+1) y]

renderTile :: Char -> Int -> Int -> Picture
renderTile c x y
 | c == 'x'  = translate x' y' $ color blue $ rectangleSolid (tileSize-1) (tileSize-1)
 | otherwise = blank
  where
    (x', y') = tileToCoord (x, y)

-- Event handling
handleKeys :: Event -> LifeGame -> LifeGame
handleKeys (EventKey (Char 'p') Down _ _) g = g {paused = not (paused g)}
handleKeys _ game = game

update :: Float -> LifeGame -> LifeGame
update secs game
 | (paused game)               = game
 | otherwise                   = updateSeconds game

updateSeconds :: LifeGame -> LifeGame
updateSeconds game = game {seconds = (seconds game) + 1}

wrapx x
 | x < 0 = maxTileHoriz
 | x > maxTileHoriz = 0
 | otherwise = x

initTiles = do 
  fileContents <- readFile "seed.txt"
  let all = words fileContents
 
  let initialState = Game { level = all, seconds = 0, paused = False }
  print all
  return initialState

main = do
  initialState <- initTiles
  play window background fps initialState render handleKeys update
