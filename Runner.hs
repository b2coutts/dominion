import System.Directory
import System.Posix.Process
import System.Posix.Files

import System.IO
import System.Random
import Data.Map
import Control.Monad
import Text.Printf
import GHC.IO.Handle.FD

import Structs
import Util
import Engine

import Base

masterSet :: Map String Card
masterSet = unions [baseSet]

baseCards :: [String]
baseCards = ["copper", "silver", "gold", "curse", "estate", "duchy", "province"]

-- hardcoded amounts for cards that should not have exactly 10 of them
amtOverrides :: Map String Int
amtOverrides = fromList
    [ ("copper"  , 60)
    , ("silver"  , 40)
    , ("gold"    , 30)
    -- , ("platinum", 12)
    -- , ("potions" , 16)
    , ("estate"  , 24)
    , ("duchy"   , 12)
    -- , ("colony"  , 12)
    -- , ("curse"   , 30)
    ]

-- the cards that a user starts with, in no particular order
startingHand :: [String]
startingHand = replicate 7 "copper" ++ replicate 3 "estate"

-- TODO: ask the users to input names/cards instead of hardcoding these values
-- TODO: add the basic cards to this list as well
userNames = ["alice", "bob"]
cardList = ["gardens", "moat", "village", "woodcutter", "smithy", "festival",
            "laboratory", "market", "cellar", "chapel"]

-- given a list of usernames and a RNG, produces a list of users and a new RNG
--  fdir - the directory containing all of the named pipes
--  rng  - the random number generator
--  u:us - the list of usernames, with input and output handles as well
newUsers :: String -> StdGen -> [(String, Handle, Handle)] -> ([User], StdGen)
newUsers _ rng [] = ([], rng)
newUsers fdir rng ((u,out,inp):us) =
    (User u (take 5 hnd) (drop 5 hnd) [] (out,inp) : rec, rng'')
    where (hnd, rng') = shuf rng startingHand
          (rec, rng'') = newUsers fdir rng' us

-- creates a new Game, given a list of user names and cards to use
-- fdir is as above
-- TODO: error handling for when a given card is not in masterSet
newGame :: String -> StdGen -> Map String Card -> [(String, Handle, Handle)]
    -> [String] -> Game
newGame fdir rng master names crds = Game{
    cards   = filterWithKey (\k v -> elem k allcards) master,
    amounts = unions [ singleton "province" (4 * length names)
                     , amtOverrides
                     , fromList $ zip crds $ repeat 10 ],
    users   = usrs,
    turn    = Turn 0 1 0 1,
    rand    = rng' }
    where (usrs, rng') = newUsers fdir rng names
          allcards = crds ++ keys amtOverrides ++ ["province"]

main = do
    -- set up named pipes
    fdir <- fmap (\pid -> "/tmp/hsdom_" ++ show pid ++ "/") getProcessID
    printf "Creating FIFOs in %s...\n" fdir
    createDirectory fdir
    ins <- forM userNames (\nm -> do
        createNamedPipe (fdir ++ nm ++ "_tousr") stdFileMode
        createNamedPipe (fdir ++ nm ++ "_tosrv") stdFileMode
        inp <- openFile (fdir ++ nm ++ "_tosrv") ReadMode
        return inp)

    printf "Establishing connection with users...\n"
    outs <- forM userNames (\nm -> do
        printf "Waiting for %s to connect..." nm
        hFlush stdout
        outp <- openFileBlocking (fdir ++ nm ++ "_tousr") WriteMode
        hSetBuffering outp LineBuffering
        printf "done.\n"
        hPrintf outp "Hi %s! Please wait while everyone else connects.\n" nm
        return outp)
    let userHandles = zip3 userNames outs ins

    -- run game
    rng <- getStdGen
    let game = newGame fdir rng masterSet userHandles cardList
    -- putStrLn $ show game ++ "\n" -- debug
    putStrLn "Starting game..."
    end <- simGame game
    putStrLn "Game is over."
    -- printf "Game over. Ending state:\n%s\n" show end

    -- clean up (TODO: somehow make this happen automatically on crash)
    removeDirectoryRecursive fdir
