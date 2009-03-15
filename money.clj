(ns money)

;TODO: 
; * Figure out what the accepted way to tag the type is
; * Figure out how to overload math operations 

(defmulti to-money 
  (fn [val] 
    (cond
      (integer? val) :int
      (float? val)   :float
      (string? val)  :string)))

(defn- make-money
  [cents]
  (with-meta {:cents cents} {:type :money}))

(defmethod to-money :int
  [cents] 
  {:cents cents})

(defmethod to-money :float
  [val] 
  {:cents (int (* val 100))})

(defmethod to-money :string
  [val] 
  (let [[all dol-str cent-str] (re-find #"([0-9]*)\.([0-9][0-9])" "123.45")]
    {:cents (+ (* 100 (Integer/parseInt dol-str)) (Integer/parseInt cent-str))}))

(defn dollars
  [money]
  (int (/ (:cents money) 100.0)))

(defn cents
  [money]
  (rem money 100))

(defn print
  [money]
  (str "$" (dollars money) "." (cents money)))
