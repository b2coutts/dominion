module Config ( startingHand, userNames, cardList ) where

-- the cards that a user starts with, in no particular order
startingHand :: [String]
startingHand = replicate 7 "copper" ++ replicate 3 "estate"
    
-- list of the users participating in the game
userNames :: [String]
userNames = ["alice", "bob"]

-- list of the kingdom cards being used in the game
cardList = ["feast", "moneylender", "remodel", "councilroom", "library",
            "smithy", "village", "market", "adventurer", "mine"]
