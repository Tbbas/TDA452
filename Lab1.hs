-- import Test.QuickCheck

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0         = 1
power n k         = n * power n (k-1)

-- Part 1

-- It takes k + 1 steps

-- Part 2

power1 :: Integer -> Integer -> Integer
power1 n k
          | k < 0     = error "power: negative argument"
          | otherwise = product (take (fromIntegral k) (repeat n))

-- Part 3

power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k
        | k < 0     = error "power: negative argument"
        | even k    = (power2 n (k `div` 2)) * (power2 n (k `div` 2))
        | otherwise = n * power2 n (k - 1)

-- Part 4
--
-- A
-- power 0 k should return 0, there is no k such that any power function returns something else than 0 given n=0,
--                           therefor it is resonable to test that this holds
-- power n 0 should return 1, there is no n such that any power function returns something else than 1 given k=0
--                           therefor it is resonable to test that this holds
-- power for arbitrary n and k should be equal to power1 and power2
-- power for odd k should be equal to power1 and power2 for odd k,
  --  seeing as this is where they differ

-- B
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = power n k == power1 n k && power n k == power2 n k

-- D

prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k
    | k >= 0 = prop_powers n k
    | otherwise = prop_powers n (abs k)

-- C
test_powers :: Bool
test_power = prop_powers 0 3 && prop_powers 3 0 && (prop_powers 4 4) && (prop_powers 4 5) && prop_powers (-1) 4
