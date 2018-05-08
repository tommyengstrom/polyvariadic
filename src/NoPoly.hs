module NoPoly where

import qualified Data.List as L


data LogLevel = Info | Error
    deriving Show

logger :: LogLevel -> String -> IO ()
logger level = putStrLn . mappend ("[" ++ show level ++ "] ")

someFunc :: IO ()
someFunc = do
    let companyId = 323 :: Int
        problemList = ["Too many chairs", "Too few tables"]
    logger Error $ "Problems for CompanyId:"  ++ show companyId ++ " "
                ++ L.intercalate ", " problemList

-- Output
-- [Error] Problems for CompanyId:323Too many chairs, Too few tables
-- [Error] Problems for CompanyId:323 Too many chairs, Too few tables
--
