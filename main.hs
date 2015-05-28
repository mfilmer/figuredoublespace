{-# LANGUAGE OverloadedStrings #-}

import Codec.Picture
import Codec.Picture.RGBA8 (fromDynamicImage)
import System.Environment (getArgs)

insertSpaces :: Image PixelRGBA8 -> Int -> Image PixelRGBA8
insertSpaces image spaceHeight = generateImage (imageSpacer image spaceHeight) width height
  where
    width = imageWidth image
    baseHeight = imageHeight image
    extraHeight = baseHeight `mod` spaceHeight
    height = (baseHeight - extraHeight) * 2 + extraHeight

imageSpacer :: Image PixelRGBA8 -> Int -> Int -> Int -> PixelRGBA8
imageSpacer baseImage spaceHeight x y
  | ifFromRaw = pixelAt baseImage x realY
  | otherwise = PixelRGBA8 0 0 0 0
  where
    ifFromRaw = y `div` spaceHeight `mod` 2 == 0
    realY = (y `div` (2 * spaceHeight)) * spaceHeight + y `mod` spaceHeight

main = do 
  args <- getArgs
  let inName = args !! 0
  let spaceHeight = read $ args !! 1
  let outName = inName ++ "-spaced.png"
  
  rawImage <- readPng (head args)
  case rawImage of
    Left errorMsg -> putStrLn errorMsg
    Right rawImage -> do
      putStrLn "Image Loaded..."
      let spacedImg = insertSpaces (fromDynamicImage rawImage) spaceHeight
      writePng outName spacedImg
      putStrLn "Spaces Added"
      