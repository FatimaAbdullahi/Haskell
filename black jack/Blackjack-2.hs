{-
Gustav Nicander, Fatima Abdullahi, Mattias Samuelsson  
-}

module Blackjack where

import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

--Task A1
--ex Hand 2
hand2 :: Hand
hand2 = [Card (Numeric 2) Hearts, Card Jack Spades]

--ex Hand 3
hand3 :: Hand
hand3 =
  [ Card Ace Spades,
    Card Ace Diamonds,
    Card King Hearts,
    Card (Numeric 5) Spades
  ]

--ex hand 4
hand4 :: Hand
hand4 = [Card Jack Spades, Card Ace Diamonds]

--ex Card
acard :: Card
acard = Card Ace Spades

--ex Card 2
secondcard :: Card
secondcard = Card (Numeric 5) Hearts

--ex Hand based on acard and secondcard
ahand :: Hand
ahand = acard : secondcard : []

--tests the size of hand and cards
--should return list of 2s
sizeSteps :: [Int]
sizeSteps =
  [ size hand2,
    size (Card (Numeric 2) Hearts : (Card Jack Spades : [])),
    (1 + size (Card Jack Spades : [])),
    1 + 1 + size [],
    1 + 1 + 0,
    2
  ]

--Task A2
--returns a string based on card input in the form of:  Rank + " of " + Suit
--data Card = Card Rank Suit
displaycard :: Card -> String
displaycard (Card (Numeric n) s) = (show n) ++ " of " ++ (show s) ++ ", "
displaycard (Card r s) = (show r) ++ " of " ++ (show s) ++ ", "

--shows all the cards in hand
display :: Hand -> String
display [] = []
display hand = displaycard (head hand) ++ display (tail hand)

-- Task A3
-- Assigns every rank a certain score value
valueRank :: Rank -> Int
valueRank Ace = 11
valueRank (Numeric n) = n
valueRank rest = 10

--returns the value of a card
valueCard :: Card -> Int
valueCard (Card r _) = valueRank r

--returns the value of a hand, also checks aces in hand
value :: Hand -> Int
value [] = 0
value hand
  | simpleSum <= 21 = simpleSum
  | otherwise = simpleSum - 10 * noa
  where
    simpleSum = sum [valueCard card | card <- hand]
    noa = recursiveaces hand

--checks amount of aces in hand, example with list comprehension
numberofaces :: Hand -> Int
numberofaces hand = sum [1 | card <- hand, rank card == Ace]

--checks amount of aces in hand, example with recursion
recursiveaces :: Hand -> Int
recursiveaces [] = 0
recursiveaces ((Card Ace _) : xs) = 1 + recursiveaces xs
recursiveaces (x : xs) = recursiveaces xs

-- checks all props
propvalue :: Hand -> Bool
propvalue hand = and [propvalue1 hand, propvalue2 hand, propvalue3 hand]

--checks if value of hand is same as same value of hand in reverse
propvalue1 :: Hand -> Bool
propvalue1 hand = value hand == value (reverse hand)

--checks if value of hand equals value of hand
propvalue2 :: Hand -> Bool
propvalue2 hand = value hand == value hand

-- checks if value of hand >= 0
propvalue3 :: Hand -> Bool
propvalue3 hand = value hand >= 0

--Task A4

-- checks if value of hand is over 21
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

--checks players hand first, checks banks hand second,
-- checks if players hand > bank hand, returns winner
winner :: Hand -> Hand -> Player
winner handp handb
  | gameOver handp = Bank
  | gameOver handb = Guest
  | value handb < value handp = Guest
  | otherwise = Bank

-- Task B1

--fulldeck generates a list of all possible card combinations
fullDeck :: Deck
fullDeck = [Card r s | r <- allranks, s <- allsuits]

--all possible suits
allsuits :: [Suit]
allsuits = [Hearts, Spades, Diamonds, Clubs]

--all possible ranks
allranks :: [Rank]
allranks =
  [rank | rank <- [Numeric n | n <- [2 .. 10]]]
    ++ [Jack, Queen, King, Ace]

-- checks if the size of fullDeck is always equal to 52
prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

--Task B2
--ex deck
deck1 :: Deck
deck1 =
  [ Card Jack Spades,
    Card Ace Hearts,
    Card (Numeric 7) Clubs,
    Card (Numeric 5) Diamonds,
    Card Jack Clubs,
    Card Queen Hearts,
    Card King Diamonds
  ]

--draws first card of deck and puts it into hand, returns (deck, hand)
draw :: Deck -> Hand -> (Deck, Hand)
draw [] _ = error "draw: The deck is empty."
draw (x : deck) hand = (deck, x : hand)

--Task B3

--bank draws a card
playBank :: Deck -> Hand
playBank deck = playbank' (deck, [])

--gives bankhand a new card from deck until the value of bankHand is 16
playbank' :: (Deck, Hand) -> Hand
playbank' (deck, bankhand)
  | value (bankhand) >= 16 = bankhand
  | otherwise = playbank' (deck', bankhand')
  where
    (deck', bankhand') = draw deck bankhand

--Task B4
-- data Rand = Rand [Double]

--picks a card from deck and returns it depending on a number between 0 and 1
cardshuffle :: Double -> Deck -> Card
cardshuffle x deck = head (drop (round (x * fromIntegral (length deck - 1))) deck)

-- shuffles deck based on random double list
shuffle :: [Double] -> Deck -> Deck
shuffle _ [] = []
shuffle [] deck = cardshuffle x deck : []
shuffle (x : xs) deck = card : shuffle xs newdeck
  where
    card = cardshuffle x deck
    newdeck = take randcard deck ++ drop (randcard + 1) deck
    randcard = (round (x * fromIntegral (length deck - 1)))

--Task B5
-- checks if a card is in a deck
belongsTo :: Card -> Deck -> Bool
c `belongsTo` [] = False
c `belongsTo` (c' : cs) = c == c' || c `belongsTo` cs

--example rand
exrandlist :: [Double]
exrandlist =
  [ 0.9,
    0.8,
    0.1,
    0.2,
    0.5,
    0.3,
    0.4,
    0.7,
    0.01,
    0.05,
    0.55,
    0.65,
    0.09,
    0.08
  ]

--checks if a card in a deck is also in the shuffled deck
prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
  card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

--checks if the length of the deck is the same as the length of the shuffled deck
prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck =
  length deck == length (shuffle randomlist deck)

--Task B6
--packages up the functions below
implementation =
  Interface
    { iFullDeck = fullDeck,
      iValue = value,
      iDisplay = display,
      iGameOver = gameOver,
      iWinner = winner,
      iDraw = draw,
      iPlayBank = playBank,
      iShuffle = shuffle
    }

-- starts game
main :: IO ()
main = runGame implementation