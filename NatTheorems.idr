module NatTheorems

import NaturalNumber

plusCommutativeZ : plus' Z' m = plus' m Z'
plusCommutativeZ {m = Z'}     = Refl
plusCommutativeZ {m = (S' k)} =
  let rec = plusCommutativeZ {m = k} in
  rewrite rec in Refl

plusCommutativeS : (k : Nat') -> (m : Nat') -> plus' (S' m) k = plus' m (S' k)
plusCommutativeS k Z'     = Refl
plusCommutativeS k (S' j) =
  let rec = plusCommutativeS k j in
  rewrite rec in Refl

total
plusCommutative' : (n : Nat') -> (m : Nat') -> plus' n m = plus' m n
plusCommutative' Z'     m = plusCommutativeZ
plusCommutative' (S' k) m =
  let rec = plusCommutative' k m in
  rewrite rec in plusCommutativeS k m

total
plusAssociative' : (n : Nat') -> (m : Nat') -> (p : Nat') -> plus' n (plus' m p) = plus' (plus' n m) p
plusAssociative' Z'     m p = ?plusAssociativeZ
plusAssociative' (S' k) m p = ?plusAssociativeS
