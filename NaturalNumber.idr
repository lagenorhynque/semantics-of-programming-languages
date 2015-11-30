module NaturalNumber

data Nat' = Z' | S' Nat'

instance Eq Nat' where
  Z'     == Z'     = True
  (S' n) == (S' m) = n == m
  _      == _      = False

instance Show Nat' where
  show Z'      = "Z'"
  show (S' Z') = "S' Z'"
  show (S' n)  = "S' (" ++ show n ++ ")"

plus' : Nat' -> Nat' -> Nat'
plus' Z'     n = n
plus' (S' n) m = plus' n (S' m)

times' : Nat' -> Nat' -> Nat'
times' Z'     _ = Z'
times' (S' n) m = plus' (times' n m) m

toNat' : Integer -> Maybe Nat'
toNat' = count Z'
  where
    count acc 0 = Just acc
    count acc n =
      if n > 0
        then count (S' acc) (n - 1)
        else Nothing

toInt :  Nat' -> Integer
toInt = count 0
  where
    count acc Z'     = acc
    count acc (S' n) = count (acc + 1) n

main : IO ()
main = putStrLn "Hello, world!"
