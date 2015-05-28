{-# LANGUAGE OverloadedStrings #-}

import Codec.Picture

imageCreator :: String -> IO ()
imageCreator path = writePng path $ generateImage pixelRenderer 250 300
   where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128

loadSampleImage path = case decodePng path of
  Left errorMsg -> putStrLn errorMsg
  Right sampleImage -> putStrLn "Loaded Successfully"

main = do 
  imageCreator "test.png"
  loadSampleImage "test.png"