(ns derivation-tree.core
  (:require [clojure.core.match :refer [match]])
  (:refer-clojure :exclude [compile])
  (:gen-class))

;;; ペアノ数

(def z nil)

(defrecord S [n])
(def s ->S)

(defn nat->str [{:keys [n] :as nat}]
  (if nat
    (str "S(" (nat->str n) ")")
    "Z"))

;;; 処理系

(defprotocol DerivationTreePrinter
  (print-tree
    [this]
    [this tab]))

(defrecord PZero [n]
  DerivationTreePrinter
  (print-tree [this]
    (print-tree this ""))
  (print-tree [_ tab]
    (let [n' (nat->str n)]
      (str tab "Z plus " n' " is " n' " by P-Zero {}"))))

(defrecord PSucc [a b c sub-tree]
  DerivationTreePrinter
  (print-tree [this]
    (print-tree this ""))
  (print-tree [_ tab]
    (let [[a' b' c'] (map nat->str [a b c])]
      (str tab a' " plus " b' " is " c' " by P-Succ {\n"
           (print-tree sub-tree (str tab "  ")) "\n"
           tab "}"))))

;;; DSL

(defmacro is [left right]
  (match [left right]
    [(['plus 'z n] :seq) _]
    `(if (= ~n ~right)
       (->PZero ~n)
       (throw (IllegalArgumentException. "not equal")))
    [(['plus (['s _] :seq) _] :seq) 'z]
    `(throw (IllegalArgumentException. "not equal"))
    [(['plus (['s n] :seq) m] :seq) (['s o] :seq)]
    `(let [premise# (is (~'plus ~n ~m) ~o)]
       (->PSucc (s ~n) ~m (s ~o) premise#))))

;;; compiler

(defn compile [prop]
  (-> prop
      print-tree
      println))

;;; example

(defn -main [& args]
  (compile (is (plus z (s (s z)))
               (s (s z))))
  (compile (is (plus (s (s z)) z)
               (s (s z)))))
