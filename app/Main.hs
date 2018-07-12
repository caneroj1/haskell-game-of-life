{-# LANGUAGE TupleSections #-}

import           Control.Monad
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid
import           Data.Vector        ((!?))
import qualified Data.Vector        as V
import           Graphics.Gloss
import           System.Environment
import           System.Random
import           Text.Read

---
--- Graphics
---

squareSize :: Float
squareSize = 25

width, height, squaresPerRow, squaresPerCol :: Int
width = 800
height = 700
squaresPerRow = floor (fromIntegral height / squareSize)
squaresPerCol = floor (fromIntegral width / squareSize)

upperLeftX, upperLeftY :: Float
upperLeftX = negate $! (fromIntegral width / 2) - (squareSize / 2)
upperLeftY = (fromIntegral height / 2) - (squareSize / 2)

upperLeft :: (Float, Float)
upperLeft = (upperLeftX, upperLeftY)

tx :: (Float, Float) -> Picture -> Picture
tx = uncurry Translate

toUpperLeft :: Picture -> Picture
toUpperLeft = tx upperLeft

rect :: Bool -> Picture
rect living =
    let rect = rectangleSolid squareSize squareSize
        in if living
            then color white rect
            else color black rect

worldToPicture :: World -> Picture
worldToPicture world = V.ifoldl' buildPictureForRow mempty world
    where
        buildPictureForRow picture row cols = picture <> V.ifoldl' (buildCellPicture row) mempty cols
        buildCellPicture row picture col cell =
            let ypos = upperLeftY + (-squareSize * fromIntegral row)
                xpos = upperLeftX + (squareSize * fromIntegral col)
                in picture `mappend` tx (xpos, ypos) (rect cell)

defaultFPS :: Int
defaultFPS = 5

---
--- Game of Life
---

type World = V.Vector (V.Vector Bool)

evolve :: World -> World
evolve world = V.imap evolveRows world
    where
        evolveRows y cols = V.imap (evolveCols y) cols
        evolveCols y x False = livingNeighbors x y world == 3
        evolveCols y x True
            | count < 2 = False
            | count < 4 = True
            | otherwise = False
            where count = livingNeighbors x y world

livingNeighbors :: Int -> Int -> World -> Int
livingNeighbors x y world = length . filter id $! mapMaybe toCell indices
    where
        indices = [(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1], (x', y') /= (x, y)]
        toCell (xloc, yloc) = (world !? yloc) >>= \v -> v !? xloc

seedWorld :: IO World
seedWorld = V.unfoldrNM squaresPerRow (\_ -> Just . (,0) <$> randomBooleanVector) 0

randomBooleanVector :: IO (V.Vector Bool)
randomBooleanVector = fmap V.fromList . sequence . take squaresPerCol $! repeat randomIO

gameOfLife :: Int -> World -> IO ()
gameOfLife fps world = simulate (InWindow "Game of Life" (width, height) (10, 10)) black fps world worldToPicture (\_ _ w -> evolve w)

staticWorld :: World -> IO ()
staticWorld world = display (InWindow "Game of Life" (width, height) (10, 10)) black (worldToPicture world)

main = do
    fps <- fromMaybe defaultFPS . join . fmap readMaybe . listToMaybe <$> getArgs
    seedWorld >>= gameOfLife fps
