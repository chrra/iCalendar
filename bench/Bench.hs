{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Criterion.Main
import           Data.ByteString.Lazy          (ByteString)
import           Data.Default

import           Text.ICalendar.Parser.Content

p :: ByteString -> [Content]
p = either error id . parseToContent def

x :: ByteString
x = "A;X=Y;Z=B:AA\r\n\tAAA\r\n"

main :: IO ()
main = defaultMain [
    bgroup "content parser" [
            bench "parse" $ nf p x
        ]
    ]
