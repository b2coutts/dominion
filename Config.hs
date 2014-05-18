module Config ( startingHand, userNames, cardList ) where

-- the cards that a user starts with, in no particular order
startingHand :: [String]
startingHand = replicate 7 "copper" ++ replicate 3 "estate"
    
-- list of the users participating in the game
userNames :: [String]
userNames = ["alice", "bob"]

-- list of the kingdom cards being used in the game
cardList = villageSquare

-- several standard configurations
firstGame = words "cellar market militia mine moat remodel smithy village\
                 \ woodcutter workshop"

bigMoney = words "adventurer bureaucrat chancellor chapel feast laboratory\
                \ market mine moneylender throneroom"

interaction = words "bureaucrat chancellor councilroom festival library\
                   \ militia moat spy thief village"

sizeDistortion = words "cellar chapel feast gardens laboratory thief village\
                      \ witch woodcutter workshop"

villageSquare = words "bureaucrat cellar festival library market remodel\
                     \ smithy throneroom village woodcutter"
