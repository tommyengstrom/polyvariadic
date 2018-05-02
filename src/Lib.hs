{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Lib
    ( someFunc
    ) where
import           Data.Proxy
import           Data.String
import           Data.Text    (Text)
import qualified Data.Text    as T
import           GHC.TypeLits

newtype Wrapped (s :: Symbol) a = Wrap {unWrap :: a}
    deriving (Show)

type UserId = Wrapped "UserId" Int
type UserName = Wrapped "UserName" String

instance IsString a => IsString (Wrapped s a) where
    fromString = Wrap . fromString

class Loggable a where
    toLog :: a -> String

instance Loggable String where
    toLog = id

instance Loggable Int where
    toLog = show

instance (KnownSymbol s, Loggable a) => Loggable (Wrapped s a) where
    toLog a = symbolVal (Proxy :: Proxy s) ++ " " ++ toLog (unWrap a)

class LogEntry a where
    logBuilder :: String -> a

instance LogEntry String where
    logBuilder = id

instance (Loggable s, LogEntry e) => LogEntry (s -> e) where
    logBuilder t = logBuilder . mappend (toLog t) . mappend " " . toLog



----------------------------------------------------------------------------

class SumRes r where
    sumOf :: Integer -> r

instance SumRes Integer where
    sumOf = id

instance (Integral a, SumRes r) => SumRes (a -> r) where
    sumOf x = sumOf . (x +) . toInteger



someFunc :: IO ()
someFunc = putStrLn "someFunc"
