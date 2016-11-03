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
--  The value of a empty hand should be 0

--hand2 = Add (Card (Numeric 2) Hearts)
--            (Add (Card Jack Spades) Empty)

handValue :: Hand -> Integer
handValue Empty = 0
handValue (Add card hand)
            | rank card == Jack || rank card == Queen || rank card == King = 10 + handValue hand
            | rank card == Ace = undefined -- Do something
            | otherwise = (extract card) + handValue hand -- How do I convert rank of card too int

extract :: Card -> Integer
extract (Card (Numeric rank) suit) = rank

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

-- Calculate if a player goes bust
-- Test Case:
--
