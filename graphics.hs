
-- Cross Correlation
smooth :: Num a => [a] -> a
smooth pixels = applyFilter' pixels [1,1,1,1,1,1,1,1,1]

gaussian :: Num a => [a] -> a
gaussian pixels = applyFilter' pixels [1,3,1,3,9,3,1,3,1]

applyFilter' pixels filter = sum (applyFilter pixels filter)

applyFilter [] [] = []
applyFilter (x:xs) (y:ys) = (x*y) : applyFilter xs ys

normalise :: Fractional a => a -> a -> a -> a
normalise max min i = ((i-min)*255)/(max-min)


-- Dithering
errorDiffusion [] error = []
errorDiffusion (pixel:pixels) error = let thresholdValue = 128
                                          pixelWithError = pixel + error
                                          newPixel = threshold pixelWithError thresholdValue
                                          newError | pixelWithError < thresholdValue = pixelWithError
                                                   | otherwise = pixelWithError - 255
                                      in 
                                          newPixel : errorDiffusion pixels newError

threshold val thresh | val < thresh = 0
                     | otherwise = 255

patternDither' image template = patternDither template p
                                where n2 = length template
                                      i = (sum image / toRational (length image)) / 255
                                      p = min (floor (i * (toRational n2 + 1))) n2

-- patternDither template 

patternDither [] p = []
patternDither (curTemplateN:template) p | p >= curTemplateN = 255 : patternDither template p
                                        | otherwise = 0 : patternDither template p

orderedDither' image template = orderedDither image template n2
                                where n2 = length template

orderedDither [] [] n2 = []
orderedDither (pixel:pixels) (curTemplateN:template) n2 | p >= curTemplateN = 255 : orderedDither pixels template n2
                                                        | otherwise = 0 : orderedDither pixels template n2
                                                         where i = pixel / 255
                                                               p = min (floor (i * (toRational n2 + 1))) n2


