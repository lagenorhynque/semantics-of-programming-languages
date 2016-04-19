module NaturalNumber

%access public export

-- 自然数(natural number)の定義
data Nat' : Type where
  Z' : Nat'
  S' : Nat' -> Nat'

Eq Nat' where
  Z'     == Z'     = True
  (S' n) == (S' m) = n == m
  _      == _      = False

Show Nat' where
  show Z'      = "Z'"
  show (S' Z') = "S' Z'"
  show (S' n)  = "S' (" ++ show n ++ ")"

-- 加法(addition)の定義
plus' : Nat' -> Nat' -> Nat'
plus' Z'     n = n
plus' (S' n) m = S' (plus' n m)

-- 乗法(multiplication)の定義
mult' : Nat' -> Nat' -> Nat'
mult' Z'     _ = Z'
mult' (S' n) m = plus' m (mult' n m)

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
