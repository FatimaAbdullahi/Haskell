-- fatima abdullahi, computer science and engineering

---------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A -------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1
 


-- B -------------------------
-- power1
{-power1 gör en lista som har k antal n element och multiplicerar 
alla element med varandra. power1 är inte definerad för k värde < 0-}
power1 :: Integer-> Integer->Integer
power1 n k | k < 0 = error "power: negative argument" 
power1 n k = product [n |_ <- [1..k]]

-- C -------------------------
-- power2
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument" 
  | k == 0         = 1
  | even k         =  power2 (n*n) (k`div`2)
  | otherwise      =  n * power2 n (k-1)

-- D -------------------------
{- 

<Describe your test cases here>
Our functions are defined for all postive whole numbers hence our test cases will range from 0 to 30.
We will also have a couple of negative numbers to verify whether or not an error message will be displayed.

-}

-- comparePower1
comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k 
      | power n k == power1 n k = True
      | power n k /= power1 n k = False
       

-- comparePower2
comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k 
      | power n k == power2 n k = True
      | power n k /= power2 n k = False

compare1n2 :: Integer -> Integer -> Bool
compare1n2 n k
   |comparePower1 n k == comparePower2 n k = True
   |comparePower1 n k /= comparePower2 n k = False

-- Test functions: 
-- we need function which takes two parameters as input and also satisfieses compare1n2
testfunc :: [Integer] -> [Integer] -> [Bool]
testfunc n k = [compare1n2 x y | x <- n, y <- k] 
testfunc [0..30][0..30]

