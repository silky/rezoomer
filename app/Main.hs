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
                                 , superimpose
                                 , RPU (..)
                                 , Border (..)
                                 , Bilinear (..)
                                 )
import           System.Random   (randomRIO)
import           Control.Monad   (replicateM)
import           Options.Generic ( getRecord
                                 , ParseRecord
                                 , Generic
                                 )
import           Data.Foldable   (foldl')



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
                      => [(Action, Region)] 
                      -> Image arr cs e
                      -> Image arr cs e
applyActionsOnRegions actionsAndRegions src = foldl' f src actionsAndRegions
    where
        f img (Zoom z, Region (x, y) (w, h)) = let  cropIt      = crop (x, y) (w, h)
                                                    zoomIt      = scale Bilinear Edge (z, z)
                                                    cropZoomed  = centerCrop (w, h)
                                                    patchIt     = flip (superimpose (x, y)) img
                                                in (patchIt . cropZoomed . zoomIt . cropIt) img


centerCrop :: Array arr cs e
           => (Int, Int)
           -> Image arr cs e
           -> Image arr cs e
centerCrop (w, h) src = crop (i, j) (w, h) src
    where
        (x, y) = dims src
        i = (x - w) `div` 2
        j = (y - h) `div` 2


go :: Options -> IO ()
go opts = do
    image <- readImageRGB RPU (inImage opts)
    
    let (x, y) = dims image
        s      = size opts
        segs a = a `div` s
        regions = [ Region (a * s, b * s) (s, s) 
                        | a <- [0..segs x - 1], b <- [0..segs y - 1] ]
 
    zooms <- replicateM (length regions) (randomRIO (1.0, 2.0))
     
    let actions = map Zoom zooms
    let final   = applyActionsOnRegions (zip actions regions) image

    writeImage (outImage opts) final


main :: IO ()
main = go =<< getRecord "rezoom - fun image transformation!"
