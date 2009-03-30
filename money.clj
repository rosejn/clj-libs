(ns money)

;TODO: 
; * Figure out what the accepted way to tag the type is
; * Figure out how to overload math operations 

(defmulti parse
  (fn [val] 
    (cond
      (integer? val) :int
      (float? val)   :float
      (string? val)  :string)))

(defn- make
  [cents]
  (with-meta {:cents cents} {:type :money}))

(defmethod parse :int
  [val] 
  (make (* 100 val)))

(defmethod parse :float
  [val] 
  (make (int (* 100 val))))

(defmethod parse :string
  [val] 
  (let [[all dol-str cent-str] (re-find #"([0-9]*)\.([0-9][0-9])" val)]
    (make (+ (* 100 (Integer/parseInt dol-str)) (Integer/parseInt cent-str)))))

(defn dollars
  [money]
  (int (/ (:cents money) 100.0)))

(defn cents
  [money]
  (rem (:cents money) 100))

(defn 
  [money]
  (str "$" (dollars money) "." (cents money)))
