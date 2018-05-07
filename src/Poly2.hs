{-# LANGUAGE FlexibleInstances #-}

module Poly2 where

class Loggable a where
    toLog :: a -> String

class LogEntry a where
    logBuilder :: Loggable s => s -> a

-- Some instances
instance Loggable String where
    toLog = id

instance Loggable Int where
    toLog = show

instance LogEntry (IO ()) where
    logBuilder = putStrLn . toLog

instance (Loggable s, LogEntry e) => LogEntry (s -> e) where
    logBuilder t = logBuilder . mappend (toLog t) . mappend " " . toLog

someFunc :: IO ()
someFunc = do
    let companyId :: Int
        companyId = 8

    logBuilder "Some issue with" companyId
-- But I still have to make it clear in the string that it's a CompanyId...

