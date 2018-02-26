module Main where

import Utmp

import Control.Monad
import Data.Semigroup ((<>))
import Data.Time
import Data.Time.Clock.POSIX
import Options.Applicative
import System.Directory

main = run $ badLogins
    <$> (option auto $ value 10
                    <> long "threshold"
                    <> short 't'
                    <> metavar "NUMBER"
                    <> help "Don't output anything less than this number of items")
    <*> (strOption $ value "/var/lib/myprog/stamp"
                    <> long "stamp"
                    <> short 's'
                    <> metavar "STAMPFILE"
                    <> help "Location of the timestamp file")
    <*> (strArguments $ value "/var/log/btmp"
                    <> metavar "LOGFILE ..."
                    <> help "Log files to be scanned")

run = join . execParser . flip info fullDesc . (helper <*>)

strArguments = some' strArgument
arguments r = some' (argument r)

some' bldr mods = (:) <$> (bldr mods) <*> many (bldr idm)

badLogins :: Int -> String -> [String] -> IO ()
badLogins limit stamppath logfiles = do
    since <- getModificationTime' stamppath
    logins <- concat <$> mapM decodeFile logfiles

    let count = length . filter ((>=since) . utTime) $ logins

    when (count >= limit) $
        print count

    writeFile stamppath $ show count

getModificationTime' f = do
    exists <- doesFileExist f
    if exists
        then getModificationTime f
        else return $ posixSecondsToUTCTime 0
