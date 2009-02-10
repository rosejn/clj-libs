(ns hash
  (:import (java.lang Integer StringBuffer)
           (java.security MessageDigest)))

(def md5-hasher (MessageDigest/getInstance "MD5"))
(def sha1-hasher (MessageDigest/getInstance "SHA-1"))

(defn- do-hash [hasher input]
  (.reset hasher)
  (.update hasher (.getBytes input))
  (.toString 
    (new BigInteger 1 (.digest hasher)) 
    16))

  (comment loop [result (new StringBuffer)
        bytes (seq (.digest hasher))]
      (if (empty? bytes)
        (str result)
        (recur 
          (.append result 
                   (Integer/toHexString (bit-and 0xff (first bytes)))) 
          (rest bytes))))

(defn md5 [input]
  (do-hash md5-hasher input))

(defn sha1 [input]
  (do-hash sha1-hasher input))
