{-# LANGUAGE CPP, BangPatterns #-}

import Utmp

import Data.Time.Format
import System.Environment

#if !MIN_VERSION_time(1,5,0)
parseTimeOrError _ = readTime
#endif

main :: IO ()
main = do
    (time:files) <- getArgs

    let !since = parseTimeOrError True defaultTimeLocale "%F %T%Q %z" time

    logins <- concat <$> mapM decodeFile files

    print . length . filter ((>=since) . utTime) $ logins
