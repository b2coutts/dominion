-- code for the base dominion set
module Base ( baseSet ) where

import qualified Data.Map as M
import Data.List
import Text.Printf

import Deck
import Structs
import Util

zero :: Either Int (Game -> Int)
zero = Left 0

-- gardens value function; gets a player's deck size modulo 10
gardensVP :: User -> Int
gardensVP (User _ hnd dck dsc _) = mod (length hnd + length dck + length dsc) 10

-- helper function for cellar; prompts the actor to discard 0 or more cards
--  n - the number of cards discarded so far
cellarAcc :: Int -> Game -> IO Game
cellarAcc n game = do
    crd <- prompt game (user $ turn $ game) msg echeck
    if crd == "/end" then drawCards n game (user $ turn $ game) else do
        aPrint game $ printf "Discarded %s.\n" crd
        oPrint game $ printf "%s discarded %s.\n" nm crd
        cellarAcc (n+1) (discard game crd)
    where (User nm hnd _ dsc oi) = actor game
          msg = printf "Your hand is %s. You have discarded %d cards. Type the\
                       \ name of the card you wish to discard, or /end to stop\
                       \ discarding." (haShow hnd) n
          echeck "/end" = Nothing
          echeck crd = if crd `elem` hnd then Nothing
            else Just $ printf "'%s' is not in your hand!" crd

-- helper function for chapel; prompts the actor to trash up to 4 cards
chapelAcc :: Int -> Game -> IO Game
chapelAcc 4 game = return game
chapelAcc n game = do
    crd <- prompt game (user $ turn $ game) msg echeck
    if crd == "/end" then return $ actDec game else do
        aPrint game $ printf "Trashed %s.\n" crd
        oPrint game $ printf "%s trashed %s.\n" nm crd
        chapelAcc (n+1) $ modActor game $ const u{hand = delete crd hnd}
    where u@(User nm hnd _ dsc oi) = actor game
          msg = printf "Your hand is %s. You have discarded %d cards. Type the\
                      \ name of the card you wish to discard, or /end to stop\
                      \ discarding." (haShow hnd) n
          echeck "/end" = Nothing
          echeck crd = if crd `elem` hnd then Nothing
              else Just $ printf "'%s' is not in your hand!" crd

chancellor :: Game -> IO Game
chancellor game = do
    resp <- prompt game (user $ turn $ game) msg yncheck
    let game' = if resp == "n" then game
        else modActor game $ const u{deck = [], disc = dck++dsc}
    simpleAct 0 2 0 0 game'
    where u@(User nm hnd dck dsc oi) = actor game
          msg = printf "Would you like to discard your deck? [y/n]"

workshop :: Game -> IO Game
workshop game@Game{cards=cs, amounts=as, turn=trn@Turn{gold=gld, buys=bs}} = do
    c <- prompt game (user $ turn $ game) msg echeck
    let game' = game{turn=trn{gold=gld + cost (cs <> c), buys=bs+1}}
    return $ actDec $ buyCard game' c
    where msg = "Choose a card costing up to 4 gold to gain."
          echeck crd = case M.lookup crd as of
            Nothing -> Just $ printf "'%s' isn't a card in this game!" crd
            Just 0  -> Just $ printf "There are no more of %s left." crd
            Just _  -> if (cost $ cs <> crd) <= 4 then Nothing else
                Just $ printf "%s is too expensive." crd

-- assumes the given game state has a feast at the top of the discard pile
feast :: Game -> IO Game
feast game@Game{cards=cs, amounts=as, turn=trn@(Turn usr bys gld act)} = do
    c <- prompt game (user $ turn $ game) msg echeck
    let game'  = game{turn=trn{gold=gld + cost (cs <> c), buys=bys+1}}
        game'' = modActor game' (\u@User{disc=d} -> u{disc=tail d})
    return $ actDec $ buyCard game'' c
    where msg = "Choose a card costing up to 5 gold to gain."
          echeck crd = case M.lookup crd as of
            Nothing -> Just $ printf "'%s' isn't a card in this game!" crd
            Just 0  -> Just $ printf "There are no more of %s left." crd
            Just _  -> if (cost $ cs <> crd) <= 5 then Nothing else
                Just $ printf "%s is too expensive." crd
    
moneylender :: Game -> IO Game
moneylender game@Game{turn=trn@Turn{gold=gld}} = do
    resp <- prompt game (user $ turn $ game) msg yncheck
    if resp == "n" then return $ actDec game else
        return $ actDec $ modActor game{turn=trn{gold=gld+3}}
                          (\u@User{hand=h} -> u{hand = delete "copper" h})
    where msg = printf "Would you like to trash a copper? [y/n]"

remodel :: Game -> IO Game
remodel game@Game{cards = cs, turn = trn} = do
    c <- prompt game (user trn) msg echeck
    let lim = (cost $ cards game <> c) + 2
        game' = modActor game (\u@User{hand=hnd} -> u{hand=delete c hnd})
        msg2 = printf "Which card would you like to gain? (max: %d gold)" lim
        echeck2 crd = case M.lookup crd (amounts game) of
            Nothing -> Just $ printf "'%s' isn't a card in this game!" crd
            Just 0  -> Just $ printf "There are no more of %s left." crd
            Just _  -> if (cost $ cards game <> crd) <= lim then Nothing else
                Just $ printf "%s is too expensive." crd
    cbuy <- prompt game' (user trn) msg2 echeck2
    let game'' = game'{turn=trn{gold=gold trn + (cost $ cs <> cbuy),
                                buys=buys trn + 1}}
    return $ actDec $ buyCard game'' cbuy
    where msg = printf "Which card would you like to remodel?"
          echeck crd = if crd `elem` (hand $ actor $ game) then Nothing else
            Just $ printf "'%s' is not in your hand!" crd

-- helper function for councilroom; draws a card for a list of user indices
drawOther :: Game -> [Int] -> IO Game
drawOther g []     = return g
drawOther g (i:is) = drawCard g i >>= flip drawOther is

councilroom :: Game -> IO Game
councilroom game@Game{users=us, turn=Turn{user=u}} = do
    game' <- drawOther game $ [0..length us - 1] \\ [u]
    simpleAct 1 0 4 0 game'

library :: Game -> IO Game
library game@Game{cards = cs, turn = Turn{user = usr}}
    | (length hnd >= 7) || (null dck && null dsc) = return $ actDec game
    | otherwise = do 
        game' <- drawCard game usr
        let c = head $ hand $ actor game'
        case (func $ cs <> c) of
            Nothing -> library game'
            Just _  -> do
                resp <- prompt game usr "Would you like to discard it? y/n"
                               yncheck
                if resp == "n" then library game'
                    else library $ modActor game' (\u@(User _ (c:cs) _ dsc' _)
                        -> u{hand = cs, disc = c:dsc'})
    where User nm hnd dck dsc oi = actor game

mine :: Game -> IO Game
mine game@Game{cards = cs, turn = trn}
    | null $ filter ((>0) . valu . (cs<>)) (hand $ actor game) = do
        aPrint game "You have no treasure cards to trash!\n"
        oPrint game $ printf "%s has no treasure cards to trash.\n"
                             (name $ actor game)
        return $ actDec game
    | otherwise = do
        c <- prompt game (user trn) msg echeck
        let lim = (cost $ cs <> c) + 3
            game' = modActor game (\u@User{hand=hnd} -> u{hand=delete c hnd})
            msg2 = printf "Which treasure would you like to gain? (max: %d gold)"
                          lim
            echeck2 crd = case M.lookup crd (amounts game) of
                Nothing -> Just $ printf "'%s' isn't a card in this game!" crd
                Just 0  -> Just $ printf "There are no more of %s left." crd
                Just _  -> if (valu $ cs <> crd) <= 0
                            then Just $ printf "%s is not a treasure card!" crd
                            else if (cost $ cs <> crd) <= lim then Nothing else
                                Just $ printf "%s is too expensive." crd
        cbuy <- prompt game' (user trn) msg2 echeck2
        let game'' = game'{turn=trn{gold = gold trn + cost (cs <> cbuy),
                                    buys = buys trn + 1}}
        return $ actDec $ buyCard game'' cbuy
    where User nm hnd dck dsc oi = actor game
          msg = "Which treasure would you like to trash?"
          echeck crd
            | not $ elem crd hnd = Just $ printf "%s is not in your hand." crd
            | (valu $ cs<>crd) <= 0 = Just $ printf "%s is not a treasure." crd
            | otherwise = Nothing

-- helper function for adventurer; n is the number of treasure cards drawn
advAcc :: Int -> Game -> IO Game
advAcc 0 game = return game
advAcc n game@Game{cards = cs} = do
    game' <- drawCard game $ user $ turn game
    let User nm hnd dck dsc oi = actor game'
        c = head hnd
    oPrint game $ printf "%s drew a %s\n" nm c
    advAcc (if valu (cs <> c) > 0 then n-1 else n) game'

adventurer :: Game -> IO Game
adventurer game = fmap actDec $ advAcc 2 game

bureaucrat :: Game -> IO Game
bureaucrat game@Game{cards = cs}
    | otherwise = do
        game' <- mapOther f game
        return $ actDec $ modActor game' (\u -> u{deck = "silver" : deck u})
    where f g u
            | any (=="moat") hnd = do
                iPrint g u $ printf "%s has a moat.\n" nm
                return g
            | any (isVictory . (cs<>)) hnd = do
                c <- prompt g u msg echeck
                iPrint g u $ printf "%s places a %s on his deck.\n" nm c
                return $ modUser g u $ const $ User nm (delete c hnd) (c:dck)
                                                   dsc oi
            | otherwise = do
                iPrint g u $ printf "%s has no victory cards: %s\n"
                                    nm (show hnd)
                return g
            where User nm hnd dck dsc oi = users g <!> u
                  msg = "Choose a victory card to place on top of your deck."
                  echeck c
                    | not $ c `elem` hnd = Just $ printf "You don't have %s!" c
                    | isVictory $ cs <> c = Nothing
                    | otherwise = Just $ printf "%s is not a victory card!" c

-- helper function for militia; attacks a given player
milAtk :: Game -> Int -> IO Game
milAtk g u
    | any (=="moat") hnd    = do
        iPrint g u $ printf "%s has a moat.\n" nm
        return g
    | length hnd <= 3       = do
        iPrint g u $ printf "%s has finished discarding.\n" nm
        hPrintf (fst oi) "You have finished discarding.\n"
        return g
    | otherwise             = do
        c <- prompt g u "Choose a card to discard." echeck
        milAtk (modUser g u $ const $ User nm (delete c hnd) dck (c:dsc) oi) u
    where User nm hnd dck dsc oi = users g <!> u
          echeck c | c `elem` hnd   = Nothing
                   | otherwise      = Just $ printf "You don't have %s!" c

militia :: Game -> IO Game
militia game = mapOther milAtk game >>= simpleAct 0 2 0 0

-- attacking helper function for spy; first arg is the user of spy
spyAtk :: Int -> Game -> Int -> IO Game
spyAtk m g u
    | any (=="moat") hnd && m /= u = do
        iPrint g u $ printf "%s has a moat.\n" nm
        return g
    | otherwise = do
        hPrintf (fst oi) "You reveal a %s.\n" c
        iPrint g u $ printf "%s revealed a %s.\n" nm c
        resp <- prompt g m (printf "Make %s discard the %s? y/n" nm c) yncheck
        case resp of
            "y" -> do
                iPrint g u $ printf "%s made %s discard their %s.\n" mnm nm c
                hPrintf (fst oi) "%s made you discard your %s.\n" mnm c
                return $ modUser g u $ const usr{disc = c:dsc, deck = cs}
            "n" -> do
                iPrint g u $ printf "%s made %s put their %s on their deck.\n"
                                    mnm nm c
                hPrintf (fst oi) "%s made you put your %s on your deck.\n" mnm c
                return g
    where mnm = name $ users g <!> m
          usr@(User nm hnd dck dsc oi) = users g <!> u
          ((c:cs, rng'), dsc') | null dck   = (shuf (rand g) dsc, [])
                               | otherwise  = ((dck, rand g), dsc)
        

spy :: Game -> IO Game
spy g = do
    let act = user $ turn g
    g' <- simpleAct 0 0 1 1 g
    mapUser (spyAtk act) [0..length (users g) - 1] g'

thiefAtk :: Int -> Game -> Int -> IO Game
thiefAtk m g@Game{cards=crds} u
    | any (=="moat") hnd = do
        iPrint g u $ printf "%s has a moat.\n" nm
        return g
    | otherwise = do
        iPrint g u $ printf "%s reveals %s and %s.\n" nm c d
        hPrintf (fst oi) "You reveal %s and %s.\n" c d
        case () of _ | valu (crds <> c) > 0 && valu (crds <> d) > 0 -> do
                        crd  <- prompt g m "Which treasure will you trash?" ech
                        keep <- fmap (=="y") $ prompt g m (printf "Will you\
                                                \ keep the %s?" crd) yncheck
                        iPrint g m $ printf "%s %ss the %s.\n" mnm (if keep then
                                            "keep" else "trashe") crd
                        let (rm,st) = if crd==c then (c,d) else (d,c)
                            g'      = modUser gr u (\x ->
                                        x{deck = ds, disc = st : disc x})
                            ch u@User{disc=p}=u{disc= if keep then rm:p else p}
                        return $ modActor g' ch
                     | valu (crds <> c) > 0 || valu (crds <> d) > 0 -> do
                        let (rm,st) = if valu (crds<>c)>0 then (c,d) else (d,c)
                        keep <- fmap (=="y") $ prompt g m (printf "Will you\
                                                \ keep the %s?" rm) yncheck
                        iPrint g m $ printf "%s %ss the %s.\n" mnm (if keep then
                                            "keep" else "trashe") rm
                        let g' = modUser gr u (\x ->
                                    x{deck = ds, disc = st : disc x})
                            ch u@User{disc=p}=u{disc = if keep then c:p else p}
                        return $ modActor g' ch
                     | otherwise -> return $ modUser gr u (\x -> x{deck = ds})
                     where ech cr
                            | cr `elem` [c,d] = Nothing
                            | True = Just$printf "You must select %s or %s." c d
    where mst = users g <!> u
          mnm = name $ users g <!> m
          usr@(User nm hnd dck dsc oi) = users g <!> u
          ((c:cs, rng'), dsc') | null dck   = (shuf (rand g) dsc, [])
                               | otherwise  = ((dck, rand g), dsc)
          ((d:ds, rng3), dsc3) | null cs    = (shuf rng' cs, [])
                               | otherwise  = ((cs, rng'), dsc')
          gr = g{rand = rng3}

thief :: Game -> IO Game
thief game = fmap actDec $ mapOther (thiefAtk $ user $ turn game) game

baseSet :: M.Map String Card
baseSet = M.fromList
  -- basic cards
  [ ("copper",   Card 0 1 zero Nothing "")
  , ("silver",   Card 3 2 zero Nothing "")
  , ("gold",     Card 6 3 zero Nothing "")
  , ("curse",    Card 0 0 (Left (-1)) Nothing "")
  , ("estate",   Card 2 0 (Left 1) Nothing "")
  , ("duchy",    Card 5 0 (Left 3) Nothing "")
  , ("province", Card 8 0 (Left 6) Nothing "")

  -- gardens are weird, yo
  , ("gardens", Card 4 0 (Right $ gardensVP . actor) Nothing $ dWrap
        "Worth 1 VP for every 10 cards in your deck (rounded down)\n")

  -- simple kingdom cards
  , ("moat",       Card 2 0 zero (jsa 0 0 2 0) $ dWrap
        "When another player plays an Attack card, you may reveal this from \
        \your hand. If you do, you are unaffected by that Attack.\n")
  , ("village",    Card 3 0 zero (jsa 0 0 1 2) $
        "+1 Card\n+2 Actions\n")
  , ("woodcutter", Card 3 0 zero (jsa 1 2 0 0) $
        "+1 Buy\n+2 Gold\n")
  , ("smithy",     Card 4 0 zero (jsa 0 0 3 0) $
        "+3 Cards\n")
  , ("festival",   Card 5 0 zero (jsa 1 2 0 2) $
        "+2 Actions\n+1 Buy\n+2 Gold\n")
  , ("laboratory", Card 5 0 zero (jsa 0 0 2 1) $
        "+2 Cards\n+1 Action\n")
  , ("market",     Card 5 0 zero (jsa 1 1 1 1) $
        "+1 Card\n+1 Action\n+1 Buy\n+1 Gold\n")

  -- complicated kingdom cards
  , ("cellar", Card 2 0 zero (Just $ cellarAcc 0) $ "+1 Action\n\n" ++ dWrap
        "Discard any number of cards. +1 Card per card discarded.\n")
  , ("chapel", Card 2 0 zero (Just $ chapelAcc 0) $ dWrap
        "Trash up to 4 cards from your hand.")
  , ("chancellor", Card 3 0 zero (Just chancellor) $ "+2 Gold\n\n" ++ dWrap
        "You may immediately put your deck into your discard pile.\n")
  , ("workshop", Card 3 0 zero (Just workshop) $ dWrap
        "Gain a card costing up to 4 gold\n")
  , ("feast", Card 4 0 zero (Just feast) $ dWrap
        "Trash this card. Gain a card costing up to 5 Gold.\n")
  , ("moneylender", Card 4 0 zero (Just moneylender) $ dWrap
        "Trash a Copper card from your hand. If you do, +3 Gold.\n")
  , ("remodel", Card 4 0 zero (Just remodel) $ dWrap
        "Trash a card from your hand. Gain a card costing up to 2 gold more\
       \ than the trashed card.\n")
  -- TODO throne room ;_;
  , ("councilroom", Card 5 0 zero (Just councilroom) $ "+4 Cards\n+1 Buy\n\n" ++
        dWrap "Each other player draws a card.")
  , ("library", Card 5 0 zero (Just library) $ dWrap
        "Draw until you have 7 cards in hand. You may set aside any Action\
       \ cards drawn this way, as you draw them; discard the set aside cards\
       \ after you finish drawing.\n")
  , ("mine", Card 5 0 zero (Just mine) $ dWrap
        "Trash a Treasure card from your hand. Gain a Treasure card costing up\
       \ to 3 gold more; put it into your hand.\n")
  , ("adventurer", Card 6 0 zero (Just adventurer) $ dWrap
        "Reveal cards from your deck until you reveal 2 Treasure cards. Put\
       \ those Treasure cards into your hand and discard the other revealed\
       \ cards.\n")

  -- TODO attack cards
  , ("bureaucrat", Card 4 0 zero (Just bureaucrat) $ dWrap
        "Gain a Silver card; put it on top of your deck. Each other player\
       \ reveals a Victory card from his hand and puts it on his deck (or\
       \ reveals a hand with no Victory cards).\n")
  , ("militia", Card 4 0 zero (Just militia) $ "+2 Gold\n\n" ++ dWrap
        "Each other player discards down to 3 cards in his hand.\n")
  , ("spy", Card 4 0 zero (Just spy) $ "+1 Card\n+1 Action\n\n" ++ dWrap
        "Each player (including you) reveals the top card of his deck and\
       \ either discards it or puts it back, your choice.\n")
  , ("thief", Card 4 0 zero (Just thief) $ dWrap
        "Each other player reveals the top 2 cards of his deck. If they\
       \ revealed any Treasure cards, they trash one of them that you choose.\
       \ You may gain any or all of these trashed cards. They discard the\
       \ other revealed cards.\n")
  ]
