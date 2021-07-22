{-# LANGUAGE CPP #-}

import Utmp

import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Foreign.Storable
import System.Environment
import System.IO
import Text.Printf

import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
    files <- getArgs

    forM_ files $ \f -> do
        bytes <- B.readFile f

        let before = B.take (fromIntegral $ sizeOf (undefined :: Utmp)) bytes
            after = encode (decode before :: Utmp)

        when (before /= after) $ hPutStrLn stderr "before /= after"

#if MIN_VERSION_binary(0,7,0)
        case runGetOrFail (many get) bytes of
            Left (_, offset, err) -> hPutStrLn stderr $ printf "%s at %d" err offset
            Right (_, _, us) -> mapM_ print (us :: [Utmp])
#else
        mapM_ print (runGet getMany bytes :: [Utmp])
#endif
