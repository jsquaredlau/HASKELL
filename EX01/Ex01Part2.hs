import Graphics.Gloss

main = animate (InWindow "Exercise One" (300, 300) (10, 10)) black (animation . round)

-- Only alter the following three definitions.

picture1 = color red $ rectangleWire 100 50

scaledPicture :: Float -> Picture
scaledPicture timeS = scale timeS timeS picture1

animation 0 = Blank
animation n = pictures [animation (n-1), scaledPicture (fromIntegral n)]
