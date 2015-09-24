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
import Data.Set (Set)
import qualified Data.Set as Set

data Options = Options  {
    optVerbose      :: Bool,
    optWriteRanking :: String -> IO (),
    optOutput       :: Handle,
    optPrecision    :: Int
}

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "o" ["output"]
        (ReqArg
            (\arg opt -> do
                handle <- openFile arg WriteMode
                return opt { optOutput = handle })
            "FILE")
        "Output file"
 
    , Option "r" ["ranking"]
        (ReqArg
            (\arg opt -> return opt { optPrecision = read arg })
            "NUMBER")
        "Order of rankings to compose the input decks with"
 
    , Option "f" ["rankingFile"]
        (ReqArg
            (\arg opt -> return opt { optWriteRanking = writeFile arg })
            "FILE")
        "File to save ranking information to (mostly for debug info)"
 
    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "Enable verbose messages"
 
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
    optWriteRanking  = (\s -> return ()),
    optOutput       = stdout,
    optPrecision    = 2     -- Default to only second order rankings
}

main = do
    args <- getArgs
 
    -- Parse options, getting a list of option actions and input deck files
    let (actions, nonOptions, errors) = getOpt RequireOrder options args

    -- Thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions
 
    let Options {
        optVerbose = verbose,
        optWriteRanking = writeRanking,
        optOutput = output,
        optPrecision = precision
    } = opts

    -- Produce a list of IO (name, contents)
    let input = sequence $ case nonOptions of
            []      -> [getContents >>= \s -> return ("stdin", s)]
            inputs  -> fmap (\i -> readFile i >>= \s -> return (i, s)) inputs

    -- =)
    when verbose (hPutStrLn stderr "Hello!")
 

    deckNamesAndContents <- input
    namedDecks <- forM deckNamesAndContents (\(name, source) -> case parseDeckString name source of
            Left err    -> fail $ show err
            Right deck  -> do
                when verbose (hPutStrLn stderr ("Parsing deck: " ++ name))
                return (name, deck))

    let decks = fmap snd namedDecks

    -- Produce the rank mappings
    ranking <- runReaderT (makeRanking precision namedDecks) verbose
    writeRanking $ dumpRanking ranking

    -- Compose the decks into the aggregate deck
    deck <- runReaderT (composeDecks ranking 60 decks) verbose
    when verbose $ hPutStrLn stderr ("Final size: " ++ (show $ Set.size deck))
    let dump = dumpDeck deck
    hPutStrLn output dump
    when verbose $ hPutStrLn stderr $ dump
    hClose output
    return ()