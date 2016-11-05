module BlackJack where
  import Cards
  import RunGame

-- At the start of the game we want to be able to retrieve a empty
-- returns a hand of type empty.
  emptyHand :: Hand
  emptyHand = Empty

--Returns the value of a specified hand
  valueHand :: Hand -> Integer
  valueHand Empty = 0
  valueHand (Add card hand) = (valueCard card) + (valueHand hand)

  --returns the real score of the given hand
  valueHand' :: Hand -> Integer
  valueHand' hand | valueHand hand > 21 = altHandValue hand
                  | otherwise = valueHand hand

-- Calculates the value of a Rank
  valueRank :: Rank -> Integer
  valueRank Ace = 11
  valueRank Jack = 10
  valueRank Queen = 10
  valueRank King = 10
  valueRank (Numeric rank) = rank

-- calculates the value of a given card
  valueCard :: Card -> Integer
  valueCard (Card rank _) = valueRank rank

-- Helper method for calculating alternative value of a hand when Ace is one.
  altHandValue :: Hand -> Integer
  altHandValue (Add card hand) =
    ((valueCard card + valueHand hand) - (10*(numberOfAces (Add card hand))))


-- Returns the number of aces in the given hand
  numberOfAces :: Hand -> Integer
  numberOfAces Empty = 0;
  numberOfAces (Add (Card Ace _) hand) = 1 + numberOfAces hand
  numberOfAces (Add card hand) = numberOfAces hand


  gameOver :: Hand -> Bool
  gameOver Empty = False
  gameOver (Add card Empty) = False;
  gameOver (Add card hand) =  altHandValue (Add card hand) >21

  winner :: Hand -> Hand -> Player
  winner guest bank | gameOver guest = Bank
  winner guest bank | gameOver bank = Guest
  winner guest bank | valueHand' bank > valueHand' guest = Bank
                    | otherwise = Guest



  --fullDeck :: Hand

--  draw :: Hand -> Hand -> (Hand,Hand)
