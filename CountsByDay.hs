import Utmp

import Control.Monad
import Data.List (sort)
import Data.List.NonEmpty (group)
import Data.Time
import System.Environment
import Text.Printf

import qualified Data.List.NonEmpty as NE

-- Reads input from utmp files

main :: IO ()
main = do
    logins <- concat <$> (mapM decodeFile =<< getArgs)
    days <- mapM (utcToLocalDay . utTime) logins

    forM_ (group . sort $ days) $ \d -> do
        let n = length d
        putStrLn $ printf "%s %d" (show $ NE.head d) n

    putStrLn $ printf "%10s %d" "" $ length days

utcToLocalDay :: UTCTime -> IO Day
utcToLocalDay t = do
    z <- getTimeZone t -- current location +/- DST at the time
    return . localDay . utcToLocalTime z $ t
