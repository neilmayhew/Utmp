{-# LANGUAGE CPP, BangPatterns #-}

import Utmp

import Control.Monad
import Data.Functor
import Data.List
import Data.Time.Format
import Data.Time.Locale.Compat
import System.Environment

#if !MIN_VERSION_time(1,5,0)
parseTimeOrError _ = readTime
#endif

main = do
    (time:files) <- getArgs

    let !since = parseTimeOrError True defaultTimeLocale "%F %T%Q %z" time

    logins <- concat <$> mapM decodeFile files

    print . length . filter ((>=since) . utTime) $ logins
