{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Utmp where

import Control.Applicative
import Control.Monad
import Data.Binary hiding (decodeFile, decodeFileOrFail)
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Data.Maybe
import Data.Ratio
import Data.Word
import Data.Text as T (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.Locale.Compat
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Tuple
import Network.Socket
import System.IO.Unsafe
import Text.Printf

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as L

#include <utmp.h>

data LoginType
    = Empty            -- 0: No valid user accounting information
    | RunLvl           -- 1: The system's runlevel
    | BootTime         -- 2: Time of system boot
    | NewTime          -- 3: Time after system clock changed
    | OldTime          -- 4: Time when system clock changed
    | InitProcess      -- 5: Process spawned by the init process
    | LoginProcess     -- 6: Session leader of a logged in user
    | UserProcess      -- 7: Normal process
    | DeadProcess      -- 8: Terminated process
    | Accounting       -- 9:
    | Undefined        -- Haskell-only
    deriving (Eq, Show, Enum)

instance Storable LoginType where
    sizeOf _    = sizeOf    (undefined :: CUShort)
    alignment _ = alignment (undefined :: CUShort)
    peek p      = toEnum . fromIntegral <$> peek (castPtr p :: Ptr CUShort)
    poke p      = poke (castPtr p :: Ptr CUShort) . fromIntegral . fromEnum

instance Binary LoginType where
    get = toEnum . fromIntegral <$> getWord16host
    put = putWord16host . fromIntegral . fromEnum

type ProcessID = Word32

data ExitStatus = ExitStatus
    { eTermination :: Word16 -- Process termination status
    , eExit        :: Word16 -- Process exit status
    } deriving (Eq, Show)

instance Storable ExitStatus where
    sizeOf _    = #{size struct exit_status}
    alignment _ = alignment (undefined :: Word16)
    peek p      = ExitStatus
        <$> (#{peek struct exit_status, e_termination} p)
        <*> (#{peek struct exit_status, e_exit       } p)
    poke p e = do
        poke (#{ptr struct exit_status, e_termination} p) (eTermination e)
        poke (#{ptr struct exit_status, e_exit       } p) (eExit        e)

instance Binary ExitStatus where
    get = ExitStatus <$> getWord16host <*> getWord16host
    put (ExitStatus t e) = putWord16host t >> putWord16host e

data Utmp = Utmp
    { utType    :: LoginType      -- Type of login
    , utPid     :: ProcessID      -- Process ID of login process
    , utLine    :: B.ByteString   -- Devicename
    , utId      :: B.ByteString   -- Inittab ID
    , utUser    :: B.ByteString   -- Username
    , utHost    :: B.ByteString   -- Hostname for remote login
    , utExit    :: ExitStatus     -- Exit status of a process marked as DeadProcess
    , utSession :: Int64          -- Session ID, used for windowing
    , utTime    :: UTCTime        -- Time entry was made
    , utAddr    :: HostAddress6   -- Internet address of remote host (first word is IPv4 addr)
    } deriving (Eq)

instance Show Utmp where
    show u = unwords . map wrap . map ($u) $
        [show . utType, show . utPid,
            unpack . utId, unpack . utUser, unpack . utLine, unpack . utHost,
            showAddr . utAddr, showTime . utTime]
      where wrap s = '[' : (s ++ "]")
            unpack = T.unpack . decodeUtf8

showAddr :: HostAddress6 -> String
showAddr (0,0,0,0) = ""
showAddr (a,0,0,0) = sockAddrHost $ SockAddrInet 0 a
showAddr  a        = sockAddrHost $ SockAddrInet6 0 0 a 0

sockAddrHost :: SockAddr -> String
sockAddrHost = fromMaybe "" . fst . unsafePerformIO . getNameInfo [NI_NUMERICHOST] True False

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%a %b %d %T %0Y %Z"

#if __WORDSIZE_TIME64_COMPAT32
type IntCompat = Int32
#else
type IntCompat = Int64
#endif

instance Storable Utmp where
    sizeOf _    = #{size struct utmp}
    alignment _ = alignment (undefined :: Int64)
    peek p      = Utmp
        <$> (                 (#{peek struct utmp, ut_type          } p))
        <*> (fromIntegral <$> (#{peek struct utmp, ut_pid           } p :: IO CUInt))
        <*> (B.packCString $  (#{ptr  struct utmp, ut_line          } p))
        <*> (B.packCString $  (#{ptr  struct utmp, ut_id            } p))
        <*> (B.packCString $  (#{ptr  struct utmp, ut_user          } p))
        <*> (B.packCString $  (#{ptr  struct utmp, ut_host          } p))
        <*> (                 (#{peek struct utmp, ut_exit          } p))
        <*> (fromIntegral <$> (#{peek struct utmp, ut_session       } p :: IO IntCompat))
        <*> (fromTimeval  <$> (#{peek struct utmp, ut_tv.tv_sec     } p :: IO IntCompat)
                          <*> (#{peek struct utmp, ut_tv.tv_usec    } p :: IO IntCompat))
        <*> (                 (#{peek struct utmp, ut_addr_v6       } p))
    poke p u = return ()

-- Null-Padded ByteStrings
getByteStringNP n = B.takeWhile (/='\0') <$> getByteString n
putByteStringNP n s = putByteString s >> replicateM_ (n - B.length s) (putWord8 0)

#if __WORDSIZE_TIME64_COMPAT32
getWordCompat = fromIntegral <$> getWord32host
putWordCompat = putWord32host . fromIntegral
#else
getWordCompat = fromIntegral <$> getWord64host
putWordCompat = putWord64host . fromIntegral
#endif

instance Binary UTCTime where
    get = fromTimeval <$> getWordCompat <*> getWordCompat
    put t = let (s, u) = toTimeval t in putWordCompat s >> putWordCompat u

fromTimeval :: (Integral n, Integral m) => n -> m -> UTCTime
fromTimeval s u = d `addUTCTime` t
  where t = posixSecondsToUTCTime (fromIntegral s)
        d = fromRational (fromIntegral u % 10^6)

toTimeval :: (Integral n, Integral m) => UTCTime -> (n, m)
toTimeval t = (s, u)
  where (s, f) = properFraction $ utcTimeToPOSIXSeconds t
        u = round $ f * 10^6

-- Don't change byte order -- needs to be the same in memory as on disk
getAddr = (,,,) <$> getWord32host  <*> getWord32host  <*> getWord32host  <*> getWord32host
putAddr (a,b,c,d) = putWord32host a >> putWord32host b >> putWord32host c >> putWord32host d

instance Binary Utmp where
    get = Utmp
        <$> get
        <*  getWord16host -- Structure alignment
        <*> getWord32host
        <*> getByteStringNP #{const UT_LINESIZE}
        <*> getByteStringNP 4
        <*> getByteStringNP #{const UT_NAMESIZE}
        <*> getByteStringNP #{const UT_HOSTSIZE}
        <*> get
        <*> getWordCompat
        <*> (fromTimeval <$> getWordCompat <*> getWordCompat)
        <*> getAddr
        <*  replicateM_ 20 getWord8 -- Reserved padding
    put u = do
        put                                     $ utType    u
        putWord16host 0 -- Structure alignment
        putWord32host                           $ utPid     u
        putByteStringNP #{const UT_LINESIZE}    $ utLine    u
        putByteStringNP 4                       $ utId      u
        putByteStringNP #{const UT_NAMESIZE}    $ utUser    u
        putByteStringNP #{const UT_HOSTSIZE}    $ utHost    u
        put                                     $ utExit    u
        putWordCompat                           $ utSession u
        put                                     $ utTime    u
        putAddr                                 $ utAddr    u
        replicateM_ 20 (putWord8 0) -- Reserved padding

#if MIN_VERSION_binary(0,7,0)

decodeFileOrFail :: FilePath -> IO (Either (ByteOffset, String) [Utmp])
decodeFileOrFail f = do
    r <- runGetOrFail (many get) <$> L.readFile f
    return $ case r of
        Left (_, offset, err) -> Left (offset, err)
        Right (_, _, us)      -> Right us

decodeFile :: FilePath -> IO [Utmp]
decodeFile f = do
    r <- decodeFileOrFail f
    case r of
        Left (_, msg) -> error msg
        Right us      -> return us

#else

getMany = do
    finished <- isEmpty
    if finished
    then return []
    else do
        next <- get
        (next:) <$> getMany

decodeFile :: FilePath -> IO [Utmp]
decodeFile f = runGet getMany <$> L.readFile f

#endif
