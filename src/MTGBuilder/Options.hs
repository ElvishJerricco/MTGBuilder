module MTGBuilder.Options where

import           System.IO

data Options = Options  {
    optVerbose      :: Bool,
    optWriteRanking :: String -> IO (),
    optOutput       :: Handle,
    optInputSeed    :: Maybe String,
    optSubtractive  :: Bool,
    optPrecision    :: Int
}
