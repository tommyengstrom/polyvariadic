{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poly5 where
import           Control.Applicative
import           Data.Maybe
import           Data.Monoid

data LogLevel = Info
              | Warning
              | Error
    deriving Show

data LogValue = LogValue
    { parts :: [String]
    , level :: Maybe LogLevel
    }
instance Monoid LogValue where
    mempty = LogValue [] Nothing
    mappend (LogValue ss l) (LogValue ss' l') = LogValue (ss <> ss') (l <|> l')

class Loggable a where
    toLog :: a -> LogValue

class Logger a where
    logger :: Loggable s => s -> a

instance Loggable LogValue where
    toLog = id

instance Loggable Int where
    toLog i = LogValue [show i] Nothing

instance Loggable String where
    toLog s = LogValue [s] Nothing

instance Loggable LogLevel where
    toLog l = LogValue [] (Just l)

instance Logger (IO ()) where
    logger a = putStrLn $ logLevel ++ unwords ss
        where
            logLevel = "[" ++ show (fromMaybe Info l) ++ "] "
            LogValue ss l = toLog a

instance (Loggable s, Logger e) => Logger (s -> e) where
    logger t = logger . mappend (toLog t) . toLog


someFunc :: IO ()
someFunc = do
    let nrProbs = 5 :: Int
    logger "There were" nrProbs "problems" :: IO ()
    logger Error "Something bad!"
