module BlackJack_Jax where
import Cards
import RunGame
import Test.QuickCheck

-- Create an empty hand
-- Test Cases:
--  An empty hand has size 0

emptyHand :: Hand
emptyHand = Empty

prop_emptyHand :: Bool
prop_emptyHand = size emptyHand == 0

-- Calculate the value of a hand
-- Test Cases:
--  Two hands of the same cards but different suits should have the same value
--  The value of the hand cannot be smaller than the size of the hand
--  The value of the hand cannot be larger than 20 + 11 (draw ace at twenty, aka draw the largest card at twenty)
--  something about numberOfAce
--  The value of a empty hand should be 0

--hand2 = Add (Card (Numeric 2) Hearts)
--            (Add (Card Jack Spades) Empty)

valueRank :: Rank -> Integer
valueRank (Numeric rank) = rank
valueRank Jack = 10
valueRank Queen = 10
valueRank King = 10
valueRank Ace = undefined

valueCard :: Card -> Integer
valueCard (Card rank _) = valueRank rank

numberOfAce :: Hand -> Integer
numberOfAce Empty = 0
numberOfAce (Add (Card Ace _) hand) = 1 + numberOfAce hand
numberOfAce (Add card hand) = numberOfAce hand

--

handValue :: Hand -> Integer
handValue Empty = 0
handValue (Add card hand) = valueCard card + handValue hand

--

handHearts :: Hand
handHearts = Add (Card (Numeric 5) Hearts)
                  (Add (Card Queen Hearts) Empty)

handSpades :: Hand
handSpades = Add (Card (Numeric 5) Spades)
                  (Add (Card Queen Spades) Empty)

prop_handValue1 :: Bool
prop_handValue1 = handHearts == handSpades

prop_handValue2 :: Hand -> Bool
prop_handValue2 hand = size hand <= handValue hand && handValue hand <= (20 + 11)

-- (Given a sorted hand ?)Calculate if a player goes bust
-- Test Case:
--  True if given a bust hand
--  a bust hand cannot beat a non-bust hand (no other hand?)
--  a hand of value 21 + a card should go bust
--  a hand of size 1 or smaller cannot go bust

gameOver :: Hand -> Bool
gameOver Empty = False
gameOver (Add card Empty) = False
gameOver (Add card hand)= undefined -- value of hand + card over 21 -> bust | must handle Ace case

--
