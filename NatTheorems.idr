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
  rewrite plusCommutativeS k j in Refl

total
plusCommutative' : (n : Nat') -> (m : Nat') -> plus' n m = plus' m n
plusCommutative' Z'     m = plusCommutativeZ
plusCommutative' (S' k) m =
  rewrite plusCommutative' k m in plusCommutativeS
