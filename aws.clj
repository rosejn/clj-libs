(ns aws
  (:import 
     (java.util ArrayList Collections HashMap List Map Set)
     java.security.SignatureException
     javax.crypto.Mac
     javax.crypto.spec.SecretKeySpec))


(def ENCODE-TABLE "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=")

(defn- encode-num
  [num]
  (let [a (bit-and num 63)
        b (bit-shift-right (bit-and num 4032) 6)
        c (bit-shift-right (bit-and num 258048) 12)
        d (bit-shift-right (bit-and num 16515072) 18)]
    (map (fn [x] (nth ENCODE-TABLE x )) (list d c b a))))

(defn- padding [ints]
  (let [ints-zero-pad (take 2 (concat ints '(0)))]
    (let [num (+ (* 256 256 (nth ints-zero-pad 0)) (* 256 (nth ints-zero-pad 1)))]
      (take 4 (concat (take (+ (count ints) 1) (encode-num num)) (repeat \=))))))

(defn- base64-encode
  "Lazily encode a sequence as base64"
  [s]
  (if s
    (let [x (map int (take 3 s))]
      (if (= 3 (count x))
        (let [num (+ (nth x 2) (* 256 (nth x 1)) (* 256 256 (first x)))]
          (lazy-cat (encode-num num) (base64-encode (drop 3 s))))
        (padding x)))))

(def MAC-TYPE "HmacSHA1")

(defn- hmac-signature 
  [secret-key data]
  (let [key (new SecretKeySpec (.getBytes secret-key) MAC-TYPE)
        mac (Mac/getInstance MAC-TYPE)]
    (.init mac key)
    (base64-encode 
      (.doFinal mac (.getBytes data)))))

(defn- keyword-to-str 
"Convert a keyword to a string without the ':' prefix."
  [k]
  (.substring (str k) 1))

(defn- make-str 
"Convert keyword argument to string if necessary."
  [val]
  (if (keyword? val)
    (keyword-to-str val)
    val))

(defn- build-signature-str
  [params]
  (reduce (fn [string [key val]] (str string (make-str key) val)) "" 
    (sort-by (fn [[key val]] (make-str key)) String/CASE_INSENSITIVE_ORDER params)))

(defn sign-request 
  [key request]
  (apply str "" (seq (hmac-signature key (build-signature-str request)))))

