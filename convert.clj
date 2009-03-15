(ns convert
  (:use clojure.contrib.test-is))

;(defmacro with-options [options defaults & body]
;  `(let [opts# (if (associative? options) 
;               options 
;               (apply hash-map options))]
;    (map (fn [[name default]] `(or (~(keyword name) ~default)))
;         opts)))
;
;(with-options options [area-code nil
;                       delimiter "-"
;                       extension nil
;                       country-code nil]
;
;  (let [number (str num)
;        options (apply hash-map args)
;        area-code    (or (:area-code options) nil)
;        delimiter    (or (:delimiter options) "-")
;        extension    (or (:extension options) nil)
;        country-code (or (:country-code options) nil)]


(defn num-to-phone [num & args]
  (let [number (str num)
        options (apply hash-map args)
        area-code    (or (:area-code options) nil)
        delimiter    (or (:delimiter options) "-")
        extension    (or (:extension options) nil)
        country-code (or (:country-code options) nil)]
    (str
      (if country-code (str "+" country-code delimiter) nil)
      (if area-code
        (.replaceFirst number "([0-9]{1,3})([0-9]{3})([0-9]{4}$)" 
                       (str "($1) " "$2" delimiter "$3"))
        (.replaceFirst number "([0-9]{1,3})([0-9]{3})([0-9]{4}$)"
                       (str "$1" delimiter "$2" delimiter "$3")))
      (if extension (str " x " extension) nil))))

(defn num-to-currency [num & args]
  (let [options (apply hash-map args)]
    
    ))

(deftest test-num-to-phone []
  (is (= "123-555-1234" 
         (num-to-phone 1235551234)))
  (is (= "(123) 555-1234" 
         (num-to-phone 1235551234 :area-code true)))
  (is (= "123 555 1234" 
         (num-to-phone 1235551234 :delimiter " ")))
  (is (= "(123) 555-1234 x 222" 
         (num-to-phone 1235551234 :area-code true :extension 222)))
  (is (= "+1-123-555-1234" 
         (num-to-phone 1235551234 :country-code 1)))
  (is (= "+1.123.555.1234 x 2222" 
         (num-to-phone 1235551234 :country-code 1 :extension 2222 :delimiter "."))))


(defn bytes-to-megabytes [n-bytes]
  (n-bytes / 1024 / 1024))
