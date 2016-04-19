module NatTheorems

import NaturalNumber

plusCommutativeZ : m = plus' m Z'
plusCommutativeZ {m = Z'}     = Refl
plusCommutativeZ {m = (S' k)} =
  let rec = plusCommutativeZ {m = k} in
  rewrite rec in Refl

plusCommutativeS : (k : Nat') -> (m : Nat') -> S' (plus' m k) = plus' m (S' k)
plusCommutativeS k Z'     = Refl
plusCommutativeS k (S' j) =
  let rec = plusCommutativeS k j in
  rewrite rec in Refl

-- 自然数の加法の可換性(commutativity)の証明
total
plusCommutative' : (n : Nat') -> (m : Nat') -> plus' n m = plus' m n
plusCommutative' Z'     m = plusCommutativeZ
plusCommutative' (S' k) m =
  let rec = plusCommutative' k m in
  rewrite rec in plusCommutativeS k m

-- 自然数の加法の結合性(associativity)の証明
total
plusAssociative' : (n : Nat') -> (m : Nat') -> (p : Nat') -> plus' n (plus' m p) = plus' (plus' n m) p
plusAssociative' Z'     m p = Refl
plusAssociative' (S' k) m p =
  let rec = plusAssociative' k m p in
  rewrite rec in Refl
