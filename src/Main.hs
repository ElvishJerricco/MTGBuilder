module Main (
    main
) where

import System.Environment
import System.IO
import System.Console.GetOpt
import System.Exit
import Control.Monad
import Control.Monad.Reader
import MTGBuilder.Deck
import MTGBuilder.Parser
import MTGBuilder.Options
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "o" ["output"]
        (ReqArg
            (\arg opt -> do
                handle <- openFile arg WriteMode
                return opt { optOutput = handle })
            "FILE")
        "Output file"

    , Option "i" ["input-seed"]
        (ReqArg
            (\arg opt -> return opt { optInputSeed = Just arg })
            "FILE")
        "Input seed file. Only applicable for the additive algorithm"

    , Option "r" ["ranking"]
        (ReqArg
            (\arg opt -> return opt { optPrecision = read arg })
            "NUMBER")
        "Order of rankings to compose the input decks with"

    , Option "f" ["ranking-file"]
        (ReqArg
            (\arg opt -> return opt { optWriteRanking = writeFile arg })
            "FILE")
        "File to save ranking information to (mostly for debug info)"

    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "Enable verbose messages"

    , Option "s" ["subtractive"]
        (NoArg
            (\opt -> return opt { optSubtractive = True }))
        "Use the subtractive algorithm"

    , Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Version 0.1.0.0"
                exitWith ExitSuccess))
        "Print version"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr (usageInfo "mtg-builder" options)
                exitWith ExitSuccess))
        "Show help"
    ]

startOptions :: Options
startOptions = Options  {
    optVerbose      = False,
    optWriteRanking = (\s -> return ()),
    optOutput       = stdout,
    optInputSeed    = Nothing,
    optSubtractive  = False,
    optPrecision    = 3     -- Default to only third order rankings
}

main = do
    -- Parse options, getting a list of option actions and input deck files
    (actions, nonOptions, errors) <- getArgs >>= return . getOpt RequireOrder options

    -- Thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions
    runReaderT (run nonOptions) opts

run :: [String] -> ReaderT Options IO ()
run files = do
    Options {
        optVerbose = verbose,
        optWriteRanking = writeRanking,
        optOutput = output,
        optInputSeed = inputSeed,
        optSubtractive = subtractive,
        optPrecision = precision
    } <- ask
    -- =)
    when verbose $ liftIO $ hPutStrLn stderr "Hello!"
    namedDecks <- sequence $ fmap parseDeckFileOrFail files
    ranking <- makeRanking precision namedDecks
    liftIO $ writeRanking $ dumpRanking ranking
    deck <- if subtractive
        then composeDecks ranking (60, 15)
        else
            let seedM = fromMaybe (return Set.empty) $ fmap ((fmap snd) . parseDeckFileOrFail) inputSeed
            in  seedM >>= composeAdditive ranking (60, 15)
    when verbose $ liftIO $ hPutStrLn stderr ("Final size: " ++ (show $ Set.size deck))
    let dump = dumpDeck deck
    liftIO $ hPutStrLn output dump
    when verbose $ liftIO $ hPutStrLn stderr dump
    liftIO $ hClose output
    return ()