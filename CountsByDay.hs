import Utmp

import Control.Monad
import Data.Function
import Data.Functor
import Data.List
import Data.Time.Clock
import System.Environment
import Text.Printf

-- Reads input from utmp files

main = do
    logins <- concat <$> (mapM decodeFile =<< getArgs)

    let days = groupBy ((==) `on` utctDay . utTime) $ logins

    forM_ days $ \ls -> do
        let n = length ls
        putStrLn $ printf "%s %d" (show . utctDay . utTime $ head ls) n

    putStrLn $ printf "%10s %d" "" $ length logins
