import Test.QuickCheck

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0         = 1
power n k         = n * power n (k-1)

-- Part 1

-- It takes k + 1 steps

-- Part 2

nListK :: Integer -> Integer -> [Integer]
nListK n k | k < 0 = error "power: negative argument"
nListK n 0         = 1 :[]
nListK n k         = n : nListK n (k - 1)

power1 :: Integer -> Integer -> Integer
power1 n k
          | k < 0     = error "power: negative argument"
          | otherwise = product (nListK n k)

-- Part 3

power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k
        | k < 0     = error "power: negative argument"
        | even k    = (power2 n (k `div` 2)) * (power2 n (k `div` 2))
        | otherwise = n * power2 n (k - 1)

-- Part 4
--
-- power 0 k should return 0
-- power n 0 should return 1
-- power for arbitrary n and k should be equal to power1 and power2
-- power for odd k should be equal to power1 and power2 for odd k,
  --  seeing as this is where they differ

prop_power n k = power n k == power1 n k && power n k == power2 n k && power n k == n^k

prop_power' n k = k >= 0 ==> (power n k == power1 n k) && (power n k == power2 n k) && (power n k == n^k)


test_power =(power 0 3 == 0) && (power 3 0 == 1) && (prop_power 4 4) && (prop_power 4 5)
