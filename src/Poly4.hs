{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poly4 where
import           Data.Proxy
import           GHC.TypeLits
import           Poly2        (LogEntry (..), Loggable (..))

newtype Wrapped (s :: Symbol) a = Wrap {unWrap :: a}
    deriving (Show)

type CompanyId = Wrapped "CompanyId" Int
type UserName = Wrapped "UserName" String

instance (KnownSymbol s, Loggable a) => Loggable (Wrapped s a) where
    toLog (Wrap i) = symbolVal (Proxy :: Proxy s) ++ ":" ++ toLog i


someFunc :: IO ()
someFunc = do
    let companyId :: CompanyId
        companyId = Wrap 8

        user :: UserName
        user = Wrap "Olle"

    logBuilder "User got a 500" user companyId
-- But what about log levels?

