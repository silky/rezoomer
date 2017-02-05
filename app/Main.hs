{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

import           Graphics.Image  ( scale
                                 , crop
                                 , readImageRGB
                                 , Image
                                 , exchange
                                 , Array
                                 , writeImage
                                 , writeImageExact
                                 , dims
                                 , leftToRight
                                 , topToBottom
                                 , RPU (..)
                                 , RGB (..)
                                 , VS (..)
                                 , Border (..)
                                 , Bilinear (..)
                                 -- , ImageFormat (..)
                                 -- , SaveOption (..)
                                 , GifDelay
                                 )
import Graphics.Image.IO.Formats
import           System.Random   (randomRIO)
import           Control.Monad   (replicateM)
import           Options.Generic ( getRecord
                                 , ParseRecord
                                 , Generic
                                 )


data Options = Options
             { size     :: Int
             , inImage  :: FilePath
             , outImage :: FilePath
             , gif      :: Bool
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


zoomImage :: Array arr cs e
          -- The IO is for randomness at the moment.
          => Options
          -> Image arr cs e
          -> IO (Image arr cs e)
zoomImage opts image = do
    let (y, x) = dims image
        s      = size opts
        segs a = a `div` s
        regions = [[ Region (a * s, b * s) (s, s) 
                        | a <- [0..segs x - 1]] | b <- [0..segs y - 1] ]
        addZoom = mapM (\ r -> do
                           z <- randomRIO (1.0, 2.0)
                           return (Zoom z, r))
    actionsRegions <- mapM addZoom regions
    --
    return $ applyActionsOnRegions actionsRegions image


writeGif :: Array arr RGB Double
         => Options
         -> Image arr RGB Double
         -> IO ()
writeGif opts image = do
    imgs <- replicateM 50 (zoomImage opts image)
    let delays = repeat 20 :: [GifDelay]
    writeImageExact GIFA
                    [GIFALooping LoopingForever]
                    (outImage opts)
                    (zip delays (map (exchange VS) imgs))


writeNormal :: Array arr RGB Double
            => Options
            -> Image arr RGB Double
            -> IO ()
writeNormal opts image = do
    final <- zoomImage opts image
    writeImage (outImage opts) final


go :: Options -> IO ()
go opts = do
    image <- readImageRGB RPU (inImage opts)
    case (gif opts) of
         True  -> writeGif opts image
         False -> writeNormal opts image


main :: IO ()
main = go =<< getRecord "rezoom - fun image transformation!"
