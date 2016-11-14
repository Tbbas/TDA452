module BlackJack_Jax where
import Cards
import RunGame
import Test.QuickCheck
import System.Random

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
valueHand :: Hand -> Integer
valueHand Empty = 0
valueHand (Add card hand) = valueCard card + valueHand hand

-- Tests valueHand
handHearts :: Hand
handHearts = Add (Card (Numeric 5) Hearts)
  (Add (Card Queen Hearts) Empty)

handSpades :: Hand
handSpades = Add (Card (Numeric 5) Spades)
  (Add (Card Queen Spades) Empty)

prop_valueHand1 :: Bool
prop_valueHand1 =  valueHand handHearts == valueHand handSpades

prop_valueHand2 :: Hand -> Bool
prop_valueHand2 hand = size hand <= valueHand hand


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
          | (valueCard card) + (valueHand hand) <= 21 = False
          | otherwise  = valueHand' (Add card hand) > 21

valueHand' :: Hand -> Integer
valueHand' Empty = 0
valueHand' (Add card hand) = ((valueCard card + valueHand hand) - (10*(numberOfAce (Add card hand))))


realHandValue :: Hand -> Integer
realHandValue hand
                | valueHand hand > 21 = valueHand' hand
                | otherwise = valueHand hand

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

-- On top of operator

(<+) :: Hand -> Hand -> Hand
(<+) Empty            hand  = hand
(<+) hand             Empty = hand
(<+) (Add card Empty) hand2 = Add card hand2
(<+) (Add card hand1) hand2 = Add card (hand1 <+ hand2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf hand1 hand2 = size (hand1 <+ hand2 ) == size hand1 + size hand2

-- full deck

fullDeck :: Hand
fullDeck = suitHand Spades <+ suitHand Hearts
          <+ suitHand Clubs <+ suitHand Diamonds

suitHand :: Suit -> Hand
suitHand suit = Empty
        <+ Add (Card (Numeric 2) suit) Empty
        <+ Add (Card (Numeric 3) suit) Empty
        <+ Add (Card (Numeric 4) suit) Empty
        <+ Add (Card (Numeric 5) suit) Empty
        <+ Add (Card (Numeric 6) suit) Empty
        <+ Add (Card (Numeric 7) suit) Empty
        <+ Add (Card (Numeric 8) suit) Empty
        <+ Add (Card (Numeric 9) suit) Empty
        <+ Add (Card (Numeric 10) suit) Empty
        <+ Add (Card Jack suit) Empty
        <+ Add (Card Queen suit) Empty
        <+ Add (Card King suit) Empty
        <+ Add (Card Ace suit) Empty

-- Draw

draw :: Hand -> Hand -> (Hand,Hand)
draw hand Empty = error "draw: The deck is empty"
draw hand (Add cardFromDeck deck) = (deck, (Add cardFromDeck hand))

-- Bank
--  hand value of playBank cannot be less than 16

playBank :: Hand -> Hand
playBank deck = playBank' Empty deck

playBank' :: Hand -> Hand -> Hand
playBank' hand deck
            | valueHand hand >= 16 = hand
            | otherwise = playBank' hand' deck'
          where
            handAndDeck = draw hand deck
            deck'       = fst handAndDeck
            hand'       = snd handAndDeck

--Shuffle
shuffle' :: StdGen -> Hand -> Hand
shuffle' gen Empty  = Empty
shuffle' gen hand = Add card' (shuffle' g1 hand')
              where
                card' = drawNthCard n1 hand
                hand' = deleteCard card' hand
                (n1,g1) = randomR (1, size hand) gen

-- Draws the nth card (from the top)
drawNthCard :: Integer -> Hand -> Card
drawNthCard _ Empty = error "Empty hand. Aborting"
drawNthCard 1 (Add card hand) = card
drawNthCard n (Add card hand) = drawNthCard (n-1) hand

-- given a hand and a card deletes that card from hand. If the card do not belong
-- the hand then return the hand unchanged.
deleteCard :: Card -> Hand -> Hand
deleteCard _ Empty = Empty
deleteCard card (Add card' hand)
                  | card == card' = hand
                  | otherwise     = (Add card' (deleteCard card hand))

-- Helper method
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h


--helper method for testing shuffle
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle' g h

-- Checks if sizes are the same after shuffle
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g Empty = True
prop_size_shuffle g hand = size hand == size (shuffle' g hand )




-- Test Hands
player_21 = Add (Card Ace Spades) (Add (Card Jack Hearts) Empty)
player_21_Ace_low = Add (Card Ace Spades) (Add (Card Jack Hearts) (Add (Card Queen Clubs) Empty))

bank_20 = Add (Card Queen Spades) (Add (Card Jack Hearts) Empty)
bank_20_Ace_low = Add (Card (Numeric 9) Spades) (Add (Card Jack Hearts) (Add (Card Ace Clubs) Empty))
bank_21_Ace_low = Add (Card Ace Spades) (Add (Card Jack Hearts) (Add (Card Queen Clubs) Empty))
