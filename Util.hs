-- utility code specifically for working with data structures from Structs
module Util ( aPrint, oPrint, actor, modActor, help, buyCard, isKingdom,
              isOver, calcVP, getWinner, drawCard, drawCards, (<>), flushHand,
              shopList, discard, cardInfo, prompt, actDec, shuf,
              finalScore, dWrap, haShow, banner ) where

import GHC.Exts
import System.IO
import System.Random
import Control.Arrow
import Text.Printf
import Data.Maybe
import Data.List
import qualified Data.Map as M

import Structs

-- Like M.!, but with a better error message
(<>) :: (Ord k, Show k, Show a) => M.Map k a -> k -> a
m <> k = case M.lookup k m of
    Just v  -> v
    Nothing -> error $ "MISS: map is " ++ show m ++ ", key is" ++ show k

-- wraps a string to n characters
dWrap :: String -> String
dWrap xs
    | length xs <= 80 = xs
    | otherwise = take ind xs ++ "\n" ++ dWrap (drop (ind+1) xs)
    where ind = maximum $ filter ((==' ') . (xs!!)) $ [0..80]

-- shows a given hand
haShow :: [String] -> String
haShow []     = "<none>"
haShow [x]    = x
haShow [x,y]  = x ++ ", and " ++ y
haShow (x:xs) = x ++ ", " ++ haShow xs

-- writes a message to the player whose turn it is
aPrint :: Game -> String -> IO ()
aPrint (Game _ _ usrs Turn{user=usr} _) msg = hPutStr (fst $ io $ usrs!!usr) msg

-- writes a message to everyone except the player whose turn it is
oPrint :: Game -> String -> IO ()
oPrint (Game _ _ usrs Turn{user=usr} _) msg =
    mapM_ (`hPutStr` msg) others
    where others = [fst $ io $ usrs!!i | i <- [0..length usrs - 1], i /= usr]

-- gets the player whose turn it is
actor :: Game -> User
actor Game{users=usrs, turn=Turn{user=usr}} = usrs !! usr

-- given a game state, "apply" the given function to the active user
modActor :: Game -> (User -> User) -> Game
modActor game@Game{users=usrs, turn=Turn{user=usr}} f =
    game{users = take usr usrs ++ [f $ usrs !! usr] ++ drop (usr+1) usrs}

-- prints a list of commands
-- TODO: finish implementing
help :: Game -> IO ()
help game = aPrint game $ unlines $
    [ "Available commands:"
    , "/hand            Displays the cards in your hand"
    , "/help            Displays this help dialog"
    , "/info <card>     Tells you what a card does"
    , "/list            Lists all cards in the shop"
    ]

-- produces a string representing info for a card, or an error message if the
-- card does not exist
cardInfo :: Game -> String -> String
cardInfo Game{cards = crds} crd = case M.lookup crd crds of
    Nothing -> printf "'%s' is not a card in this game.\n" crd
    Just (Card cst valu vp fn dscr) -> unlines $
        let cname = crd ++ case fn of Nothing -> ""
                                      Just _  -> " (ACTION)" in
            [ replicate (length cname) '-'
            , cname
            , replicate (length cname) '-'
            , printf "Cost: %d" cst
            , printf "Value: %d" valu
            , case vp of Left n  -> printf "VPs: %d\n" n
                         Right _ -> "VPs: ?\n"
            , dscr ]

-- returns a new game state where the active user has purchased the given card
buyCard :: Game -> String -> Game
buyCard game@Game{turn=trn@(Turn usr bys gld act), amounts = amts} c =
    modActor game' (\u -> u{disc = c : disc u})
    where cardCost = cost $ cards game <> c
          amt   = amts <> c
          game' = game{amounts = M.insert c (amt-1) amts,
                       turn = trn{buys = bys-1, gold = gld-cardCost}}

-- Checks whether or not a given card is a kingdom card
-- TODO: maybe use a better way of checking this? TODO more than base
isKingdom :: String -> Bool
isKingdom str = not $ elem str ["copper", "silver", "gold", "curse", "estate",
                                "duchy", "province"]

-- true iff the given game is over (endgame conditions have been met)
isOver :: Game -> Bool
isOver game@(Game crds amts usrs (Turn usr buys gold acts) _) =
    amts <> "province" == 0 ||
    (M.size $ M.filterWithKey (\k v -> isKingdom k && v == 0) amts) >= 3

-- calculates the victory points of a single card in a game
cardVP :: Game -> Card -> Int
cardVP _ Card{vps = Left n} = n
cardVP game Card{vps = Right f} = f game

-- calculates the victory points for a single player in the game
calcVP :: Game -> Int -> Int
calcVP game@Game{cards = crds, users = usrs} usr =
    sum $ map (cardVP game' . (crds <>)) allcards
    where game' = game{turn = Turn usr 0 0 0}
          allcards = concatMap ($ (usrs !! usr)) [hand, disc, deck]

-- calculates victory points and produces the winner or the game, and their VPs
-- TODO: ties
getWinner :: Game -> (Int, Int)
getWinner game =
    foldr (\x y@(_,vp) -> if calcVP game x > vp then (x, calcVP game x) else y)
          (0, -99999) [0..length (users game) - 1]

-- creates a display of the final scores in the game
finalScore :: Game -> String
finalScore game@Game{users=usrs} = unlines $ ["Player              Score"] ++
    map (\(nm,vp) -> printf "%-20s%d" nm vp) (sortWith snd scores)
    where scores = [(name $ usrs!!i, calcVP game i) | i <- [0..length usrs - 1]]

-- draw a single card for the given user
drawCard :: Game -> Int -> IO Game
drawCard game@Game{users=usrs} usr
    | null dck && null dsc = do
        hPrintf h "You have no cards to draw!\n"
        return game
    | otherwise = do
        hPrintf h "You draw a %s.\n" c
        return game{rand = rng', users = usrs'}
    where u@(User _ hnd dck dsc (h,_)) = users game !! usr
          ((c:cs, rng'), dsc') = if null dck then (shuf (rand game) dsc, [])
                                             else ((dck, rand game), dsc)
          usrs' = take usr usrs ++ [u{hand = c:hnd, deck = cs, disc = dsc'}] ++
                  drop (usr+1) usrs

-- draws n cards for the given user
--  n - the number of cards to draw
drawCards :: Int -> Game -> Int -> IO Game
drawCards 0 game _ = return game
drawCards n game usr = do
    game' <- drawCard game usr
    drawCards (n-1) game' usr

-- transfer the given user's hand into their discard pile, then draw 5 cards
flushHand :: Game -> Int -> Game
flushHand game@Game{rand=rng} usr = modActor game{rand = rng'} $ const
    u{hand = take 5 dck', deck = drop 5 dck', disc = dsc'}
    where u@(User _ hnd dck dsc _) = actor game
          ((dck', rng'), dsc') = if length dck >= 5 then ((dck,rng), hnd++dsc)
                else (first (dck++) $ shuf rng (hnd++dsc), [])

-- create a human-readable list of all cards being sold
shopList :: Game -> String
shopList (Game crds amts _ _ _) = "Card                Cost    Amount\n" ++
    concatMap (\c -> printf "%-20s%-8d%d\n" c (cost $ crds M.! c) (amts M.! c))
              (sortWith (cost . (crds M.!)) (M.keys amts))

-- discards one of the given card from the active player's hand
discard :: Game -> String -> Game
discard game c = modActor game (\u@(User _ h _ d _) -> u{hand = delete c h,
                                                         disc = c:d})

-- prompt a user for input with error checking. Also handles special commands
--  usr - the index of the user in Game{users}
--  msg - a message with which to prompt the user
--  fn  - a function which is applied to the user's response. If it produces
--        Nothing, then Just the user's response is returned. If it produces
--        Just err, then the error message err is printed to the user, and
--        they are reprompted for input.
prompt :: Game -> Int -> String -> (String -> Maybe String) -> IO String
prompt game usr msg fn = do
    hPutStrLn out msg
    resp <- hGetLine inp
    case words resp of
        "/hand":_   -> (aPrint game $ printf "Your hand is: %s.\n" (haShow hnd))
                       >> redo
        "/list":_   -> (aPrint game $ shopList game) >> redo
        ["/info"]   -> (aPrint game $ "Usage: /info <card>\n") >> redo
        "/info":c:_ -> (aPrint game $ cardInfo game c) >> redo
        "/help":_   -> help game >> redo
        _ -> case fn resp of Nothing  -> return resp
                             Just err -> hPutStrLn out err >> redo
    where redo = prompt game usr msg fn
          User nm hnd dck dsc (out, inp) = users game !! usr

-- decreases the number of actions in the game by 1
actDec :: Game -> Game
actDec game@Game{turn=trn@Turn{acts=n}} = game{turn=trn{acts = n-1}}

-- makes an info banner for the active user, displaying their hand, buys, etc
banner :: Game -> String
banner game = printf "Buys: %-4d Gold: %-4d Actions: %-4d Total Cards: %d\n\
                     \Hand: %s\n" bys gld act (length $ hnd ++ dck ++ dsc)
                                  (haShow hnd)
    where u@(User _ hnd dck dsc (h,_)) = users game !! usr
          Turn usr bys gld act = turn game

-- randomly permutes a list
shuf :: StdGen -> [a] -> ([a], StdGen)
shuf gen [] = ([], gen)
shuf gen (x:xs) = (take ind rec ++ [x] ++ drop ind rec, gen'')
    where (n, gen') = random gen :: (Int, StdGen)
          ind = mod n $ length xs + 1
          (rec, gen'') = shuf gen' xs
