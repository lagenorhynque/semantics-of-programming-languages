module NaturalNumber where

data Nat = Z | S Nat deriving (Eq, Show)

infixl 6 `plus`
plus :: Nat -> Nat -> Nat
Z   `plus` n = n
S n `plus` m = n `plus` S m

infixl 7 `times`
times :: Nat -> Nat -> Nat
Z   `times` _ = Z
S n `times` m = n `times` m `plus` m

toNat :: Int -> Maybe Nat
toNat = count Z
  where
    count acc 0 = Just acc
    count acc n
      | n > 0     = count (S acc) (n - 1)
      | otherwise = Nothing

toInt :: Nat -> Int
toInt = count 0
  where
    count acc Z     = acc
    count acc (S n) = count (acc + 1) n

main :: IO ()
main = do
  putStrLn $ show a ++ " plus " ++ show b ++ " is " ++ show (a `plus` b)
  putStrLn $ show a ++ " times " ++ show b ++ " is " ++ show (a `times` b)
    where
      a = S (S Z)
      b = S (S (S Z))
