-- Lecture notes/code for lecture 1 on 31/10 -16

--The following code is from the live demo.


import Test.QuickCheck

-- exchangeRate Calculates SEK to EUR and vice versa
exchangeRate = 9.8963  -- SEK / EUR

toEUR sek = sek / exchangeRate

toSEK eur = eur * exchangeRate


prop_exchange sek = toSEK (toEUR sek) ~== sek

x ~== y = abs (x-y) < 1e-10


-- * Definition by cases
absolute2  x | x<0 = -x
             | x>=0 = x

absolute1 x = if x<0 then -x else x

-- Power function by recursion

power n 0     = 1
power n k     = power n (k-1) * n


--Power function that checks for positive k

power' n 0     = 1
power' n k   | k>0 = power n (k-1) * n
power' n k   | k<0 = 1 / power n (-k) --Does not work


-- Boolean check to make sure our result
--is the same as the stock power function
prop_power n k = power n k == n^k


-- intersecting lines

-- * Tuples

--examplePair :: (Bool, Double)
--examplePair = (True, 3.14)

--exampleTriple :: (Bool, Int, String)

--exampleTriple = (False, 42, "Answer")

--exampleFunction :: (Bool, Int, String) -> Bool


-- * List

--snacks = "Spam"

--dinner = [snacks, "Fish", "Chips", snacks, snacks, "Pudding"]

--summary :: [String] -> String

-- | Computing the length of a list
-- len

-- last'

-- * List comprehensions

--ex1

--doubles xs

--ex2 (multiple generators)

--pythag
