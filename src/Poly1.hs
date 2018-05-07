{-# LANGUAGE FlexibleInstances #-}

module Poly1 where

class LogEntry a where
    logBuilder :: String -> a

instance LogEntry (IO ()) where
    logBuilder = putStrLn

instance (LogEntry e) => LogEntry (String -> e) where
    logBuilder t = logBuilder . mappend t . mappend " "

someFunc :: IO ()
someFunc = do
    let errorString = "I got access denied or something" :: String
        nrOfThings = 8 :: Int
    logBuilder errorString "Current number of things:" (show nrOfThings)

-- But it's annoying having to make each part of it a string...

