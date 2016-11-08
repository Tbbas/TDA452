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
--  The value of a empty hand should be 0
handValue :: Hand -> Integer
handValue Empty = 0
handValue (Add card hand) = valueCard card + handValue hand

-- Tests handValue
handHearts :: Hand
handHearts = Add (Card (Numeric 5) Hearts)
  (Add (Card Queen Hearts) Empty)

handSpades :: Hand
handSpades = Add (Card (Numeric 5) Spades)
  (Add (Card Queen Spades) Empty)

prop_handValue1 :: Bool
prop_handValue1 =  handValue handHearts == handValue handSpades

prop_handValue2 :: Hand -> Bool
prop_handValue2 hand = size hand <= handValue hand


-- given a rank calculates the value
valueRank :: Rank -> Integer
valueRank (Numeric rank) = rank
valueRank Jack = 10
valueRank Queen = 10
valueRank King = 10
valueRank Ace = 11

-- given a card calculates the value
valueCard :: Card -> Integer
valueCard (Card rank _) = valueRank rank

-- given a  hand calualtes the number of aces in it
numberOfAce :: Hand -> Integer
numberOfAce Empty = 0
numberOfAce (Add (Card Ace _) hand) = 1 + numberOfAce hand
numberOfAce (Add card hand) = numberOfAce hand

-- Given a hand, Calculate if a player goes bust
-- Test Case:
--  True if given a bust hand
--  a bust hand cannot beat a non-bust hand (no other hand?)
--  a hand of value 21 + a card should go bust
--  a hand of size 1 or smaller cannot go bust

gameOver :: Hand -> Bool
gameOver Empty = False
gameOver (Add card Empty) = False
gameOver (Add card hand)
          | (valueCard card) + (handValue hand) <= 21 = False
          | otherwise  = handValue' (Add card hand) > 21

handValue' :: Hand -> Integer
handValue' Empty = 0
handValue' (Add card hand) = ((valueCard card + valueHand hand) - (10*(numberOfAces (Add card hand))))


realHandValue :: Hand -> Integer
realHandValue hand
                | handValue hand > 21 = handValue' hand
                | otherwise = handValue hand

prop_gameOver :: Hand -> Bool
prop_gameOver h | realHandValue h > 21 = gameOver h
prop_gameOver h = gameOver h == False


prop_gameOver' :: Card -> Bool
prop_gameOver' c = gameOver (Add c player_21_Ace_low)

prop_gameOver'' :: Card -> Bool
prop_gameOver'' c = gameOver(Add c Empty) == False

-- Given a hand, returns Guest if the first hand wins, returns Bank if the second hand wins.
-- The second hand wins if the hands are equal.
-- Test Case:
--  A bust hand cannot win
--  if given the same hand as both argument?

winner :: Hand -> Hand -> Player
winner Empty _                                      = Bank
winner _ Empty                                      = Guest
winner player bank
      | gameOver player                             = Bank
      | gameOver bank                               = Guest
      | realHandValue player  >  realHandValue bank = Guest
      | otherwise                                   = Bank

prop_winner :: Card -> Bool
prop_winner card = (winner (Add card player_21_Ace_low) bank_21_Ace_low  == Bank)
                && (winner player_21_Ace_low (Add card bank_21_Ace_low) == Guest)





-- Test Hands
player_21 = Add (Card Ace Spades) (Add (Card Jack Hearts) Empty)
player_21_Ace_low = Add (Card Ace Spades) (Add (Card Jack Hearts) (Add (Card Queen Clubs) Empty))

bank_20 = Add (Card Queen Spades) (Add (Card Jack Hearts) Empty)
bank_20_Ace_low = Add (Card (Numeric 9) Spades) (Add (Card Jack Hearts) (Add (Card Ace Clubs) Empty))
bank_21_Ace_low = Add (Card Ace Spades) (Add (Card Jack Hearts) (Add (Card Queen Clubs) Empty))
