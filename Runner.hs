import System.IO
import System.Random
import Data.Map

import Misc
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
    , ("platinum", 12)
    , ("potions" , 16)
    , ("estate"  , 24)
    , ("duchy"   , 12)
    , ("colony"  , 12) -- TODO: is this right?
    , ("curse"   , 30) ]

-- the cards that a user starts with, in no particular order
startingHand :: [String]
startingHand = replicate 7 "copper" ++ replicate 3 "estate"

-- TODO: ask the users to input names/cards instead of hardcoding these values
userNames = ["Bryan"]
cardList = ["gardens", "moat", "village", "woodcutter", "smithy", "festival",
            "laboratory", "market"]

-- given a list of usernames and a RNG, produces a list of users and a new RNG
newUsers :: StdGen -> [String] -> ([User], StdGen)
newUsers rng [] = ([], rng)
newUsers rng (u:us) = (User u (take 5 hnd) (drop 5 hnd) [] handle : rec, rng'')
    where (hnd, rng') = shuf rng startingHand
          (rec, rng'') = newUsers rng' us
          handle = (stdout, stdin) -- TODO: actually do this

-- creates a new Game, given a list of user names and cards to use
-- TODO: error handling for when a given card is not in masterSet
newGame :: StdGen -> Map String Card -> [String] -> [String] -> Game
newGame rng master names crds = Game{
    cards   = filterWithKey (\k v -> elem k crds) master,
    amounts = filterWithKey (\k v -> elem k crds) $
                unions [ singleton "province" (4 * length names)
                       , amtOverrides
                       , fromList $ zip crds $ repeat 10 ],
    users   = usrs,
    turn    = Turn 0 1 0 1,
    rand    = rng' }
    where (usrs, rng') = newUsers rng names

main = do
    rng <- getStdGen
    let game = newGame rng masterSet userNames cardList
    simGame game
