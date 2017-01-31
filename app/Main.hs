{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main where

import           Graphics.Image  ( scale
                                 , crop
                                 , readImageRGB
                                 , Image
                                 , Array
                                 , writeImage
                                 , dims
                                 , leftToRight
                                 , topToBottom
                                 , RPU (..)
                                 , Border (..)
                                 , Bilinear (..)
                                 )

import           System.Random   (randomRIO)

import           Options.Generic ( getRecord
                                 , ParseRecord
                                 , Generic
                                 )




data Options = Options
             { size     :: Int
             , inImage  :: FilePath
             , outImage :: FilePath
             } deriving (Show, Generic)


instance ParseRecord Options


type X      = Int
type Y      = Int
type Height = Int
type Width  = Int


data Region = Region !(X, Y) !(Width, Height) 
                deriving Show


data Action = Zoom !Double 
                deriving Show


applyActionsOnRegions :: Array arr cs e 
                      => [[(Action, Region)]]
                      -> Image arr cs e
                      -> Image arr cs e
applyActionsOnRegions actionsAndRegions src = combine $ map (map (f src)) actionsAndRegions
    where
        combine = foldr1 topToBottom . map (foldr1 leftToRight)
        f img (Zoom z, Region (x, y) (w, h)) = let  cropIt      = crop (y, x) (h, w)
                                                    zoomIt      = scale Bilinear Edge (z, z)
                                                    cropZoomed  = centerCrop (h, w)
                                                in (cropZoomed . zoomIt . cropIt) img


centerCrop :: Array arr cs e
           => (Int, Int)
           -> Image arr cs e
           -> Image arr cs e
centerCrop (h, w) src = crop (i, j) (h, w) src
    where
        (m, n) = dims src
        i = (m - h) `div` 2
        j = (n - w) `div` 2


go :: Options -> IO ()
go opts = do
    image <- readImageRGB RPU (inImage opts)
    
    let (y, x) = dims image
        s      = size opts
        segs a = a `div` s
        regions = [[ Region (a * s, b * s) (s, s) 
                        | a <- [0..segs x - 1]] | b <- [0..segs y - 1] ]
        addZoom = mapM (\ r -> do
                           z <- randomRIO (1.0, 2.0)
                           return (Zoom z, r))
    actionsRegions <- mapM addZoom regions
     
    let final   = applyActionsOnRegions actionsRegions image

    writeImage (outImage opts) final


main :: IO ()
main = go =<< getRecord "rezoom - fun image transformation!"
