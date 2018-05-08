{-# LANGUAGE FlexibleInstances #-}

module Poly3 where

import           Poly2 (LogEntry (..), Loggable (..))

newtype CompanyId = CompanyId Int
    deriving Show

instance Loggable CompanyId where
    toLog = show

someFunc :: IO ()
someFunc = do
    let companyId :: CompanyId
        companyId = CompanyId 4

    logBuilder "Some issue with" companyId

-- Output:
-- Some issue with CompanyId 4


-- But defining new instances for every type is a lot of work!

