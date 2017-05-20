{-# LANGUAGE NamedFieldPuns #-}

import           Data.Time
import           Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = do
    zt <- getZonedTime
    let time =
            realToFrac $ timeOfDayToTime $ localTimeOfDay $
            zonedTimeToLocalTime zt
    play display white refreshRate time draw onEvent onTick
  where
    display = InWindow "haskarium" (800, 600) (0, 0)
    refreshRate = 60

type World = Float

draw :: World -> Picture
draw time = pictures $ numbers ++ hands
  where
    secs = fromIntegral (round time :: Int)
    mins = secs / 60
    hours = mins / 60
    hands = [hand secs 200, hand mins 150, hand (5 * hours) 100]
    hand angle len = rotate (6 * angle) $ line [(0, 0), (0, len)]
    numbers =
        [ rotate (fromIntegral $ n * 30) $ translate dx 200 $ scale 0.2 0.2 $
          text $ show n
        | n <- [1 .. 12 :: Int]
        , let dx = if n < 10 then -7.5 else -15
        ]

onEvent :: Event -> World -> World
onEvent _ world = world

onTick :: Float -> World -> World
onTick dt time = time + dt
