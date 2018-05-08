{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poly5 where
import           Control.Applicative
import qualified Data.List           as L
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           GHC.TypeLits

data LogLevel = Info | Error
    deriving Show

-- New data structure containing extra information
data LogValue = LogValue
    { parts :: [String]
    , level :: Maybe LogLevel
    }
instance Monoid LogValue where
    mempty = LogValue [] Nothing
    mappend (LogValue ss l) (LogValue ss' l') = LogValue (ss <> ss') (l <|> l')

class ToLog a where
    toLog :: a -> LogValue

class Logger a where
    logger :: ToLog s => s -> a

newtype Wrapped (s :: Symbol) a = Wrap {unWrap :: a}
    deriving (Show)

type CompanyId = Wrapped "CompanyId" Int

instance (KnownSymbol s, ToLog a) => ToLog (Wrapped s a) where
    toLog (Wrap i) =
        let logStr  = symbolVal (Proxy :: Proxy s) ++ ":" ++ baseStr
            baseStr = head . parts $ toLog i
         in LogValue [logStr] Nothing

instance ToLog LogValue where
    toLog = id

instance ToLog Int where
    toLog i = LogValue [show i] Nothing

instance ToLog String where
    toLog s = LogValue [s] Nothing
instance ToLog [String] where
    toLog l = LogValue [L.intercalate ", " l] Nothing

instance ToLog LogLevel where
    toLog l = LogValue [] (Just l)

instance Logger (IO ()) where
    logger a = putStrLn $ logLevel ++ unwords ss
        where
            logLevel = "[" ++ show (fromMaybe Info l) ++ "] "
            LogValue ss l = toLog a

instance (ToLog s, Logger e) => Logger (s -> e) where
    logger t = logger . mappend (toLog t) . toLog


someFunc :: IO ()
someFunc = do
    let companyId = Wrap 323 :: CompanyId
        problemList = ["Too many chairs", "Too few tables"] :: [String]

    logger Error "Problems for" companyId problemList

-- Output:
-- [Error] Problems for CompanyId:323 Too many chairs, Too few tables

