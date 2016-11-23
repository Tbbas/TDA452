module BlackJack where
import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)
import System.Random

implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }


main :: IO ()
main = runGame implementation

-- hand2
{-
size hand2
  = size (Add (Card (Numeric 2) Hearts)
              (Add (Card Jack Spades) Empty))
  = 1 + size (Add (Card Jack Spades) Empty)
  = 1 + (1 + size Empty)
  = 1 + 1 + 0
  = 2
-}

-- Create an empty hand

empty :: Hand
empty = Empty


-- Calculates the value of a hand, either by seeing the aces as 11 or seeing them as 1.
value :: Hand -> Integer
value hand
                | handValue > 21 = handValue - (10*(numberOfAce hand))
                | otherwise = handValue
                where
                  handValue = normalValue hand


-- Calculates the value of a hand when all Aces are seen as 11
normalValue :: Hand -> Integer
normalValue Empty = 0
normalValue (Add card hand) = valueCard card + normalValue hand



-- Given a rank calculates the value
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

gameOver :: Hand -> Bool
gameOver Empty = False
gameOver hand = value hand > 21




-- Given a hand, returns Guest if the first hand wins, returns Bank if the second hand wins.
-- The second hand wins if the hands are equal.

winner :: Hand -> Hand -> Player
winner player bank
      | gameOver player                             = Bank
      | gameOver bank                               = Guest
      | value player  >  value bank                 = Guest
      | otherwise                                   = Bank

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

-- Generates a full deck.
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
draw Empty hand = error "draw: The deck is empty"
draw (Add cardFromDeck deck) hand = (deck, (Add cardFromDeck hand))

-- Bank
--  hand value of playBank cannot be less than 16

playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

playBank' :: Hand -> Hand -> Hand
playBank'  deck hand
            | value hand >= 16 = hand
            | otherwise = playBank' hand' deck'
          where
            handAndDeck = draw deck hand
            deck'       = snd handAndDeck
            hand'       = fst handAndDeck

--Shuffle
shuffle :: StdGen -> Hand -> Hand
shuffle gen Empty  = Empty
shuffle gen hand = Add card' (shuffle g1 hand')
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
    c `belongsTo` h == c `belongsTo` shuffle g h

-- Checks if sizes are the same after shuffle
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g Empty = True
prop_size_shuffle g hand = size hand == size (shuffle g hand )
