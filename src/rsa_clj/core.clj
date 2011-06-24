(ns rsa-clj.core
  (:use [clojure.contrib.math :only (floor expt)]))

(def *p* 104729)
(def *q* 105071)

(defn third [coll]
  (second (rest coll)))

(defn mod-mult [a b m]
  (mod (mod a m) (mod b m) m))

(defn mod-exp [base e n]
  "Montgomery exponentiation mod m."
  )
  

(defn gcd [a b]
  (if (< a b) (gcd b a)
      (loop [a a b b]
	(cond (zero? b) a
	      :else (recur b (mod a b))))))

(defn bezouts-id [a b]
  "Returns [m n] such that m*a + n*b = (a,b)."
  (if (zero? (mod a b)) [0 1]
      (let [[x y] (bezouts-id b (mod a b))]
	[y (- x (* y (quot a b)))])))

(defn phi [p q]
  "Returns phi(n), where n = p*q (p,q prime) and phi is the Euler Phi Function."
  (* (dec p) (dec q)))

(defn make-encrypt-exp [n phi]
  (loop [e (floor (* (rand) n))]
    (if (and (> e (/ n 3))
	     (= 1 (gcd e phi)))
      e
      (recur (floor (* (rand) n))))))

(defn make-decrypt-exp [e phi]
  (first (bezouts-id e phi)))

(defn encrypt [e n m]
  (mod-expt (expt m e) n))

(defn decrypt [d n m]
  (encrypt d n m))
