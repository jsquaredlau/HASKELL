import Graphics.Gloss

main = display (InWindow "Exercise One" (200, 200) (10, 10)) black picture2

-- Only alter the following two definitions.

picture1 = color red $ rectangleWire 100 50

picture2 = pictures [color green $ rectangleSolid 200 100,picture1 ]
