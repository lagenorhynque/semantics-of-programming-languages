module NaturalNumber where

data Nat = Z | S Nat deriving (Eq, Show)

infixl 6 `plus`
plus :: Nat -> Nat -> Nat
Z   `plus` n = n
S n `plus` m = S (n `plus` m)

infixl 7 `mult`
mult :: Nat -> Nat -> Nat
Z   `mult` _ = Z
S n `mult` m = m `plus` n `mult` m

toNat :: Integer -> Maybe Nat
toNat = count Z
  where
    count acc 0 = Just acc
    count acc n
      | n > 0     = count (S acc) (n - 1)
      | otherwise = Nothing

toInteger' :: Nat -> Integer
toInteger' = count 0
  where
    count acc Z     = acc
    count acc (S n) = count (acc + 1) n

main :: IO ()
main = do
  putStrLn $ show a ++ " plus " ++ show b ++ " is " ++ show (a `plus` b)
  putStrLn $ show a ++ " mult " ++ show b ++ " is " ++ show (a `mult` b)
  where
    a = S (S Z)
    b = S (S (S Z))
