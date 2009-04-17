(ns auth.digest
  (:import (java.security MessageDigest)))

; Using a unique site key in the digest helps keep passwords safer from dictionary attacks on weak passwords.
(def *digest-site-key*      "asdf134asdf")

; Stretching, running the digest multiple times, makes it harder to brute force passwords also.  5 stretches means 5x the work.
(def DIGEST-NUM-STRETCHES 10)

(defn hex [bytes]
  (reduce (fn [result byte] 
            (str result (.substring 
                   (Integer/toString (+ (bit-and byte 0xff) 0x100) 16)
                   1)))
          "" bytes))

(defn- do-hash [type input]
  (let [hasher (MessageDigest/getInstance type)]
    (hex (.digest hasher (.getBytes input)))))

(defn md5 [input]
  (do-hash "MD5" input))

(defn sha1 [input]
  (do-hash "SHA-1" input))

(defn secure-digest [& stuff]
  (sha1 (apply str (interpose "--" stuff))))

(defn make-salt []
  (let [time   (.getTime (java.util.Date. ))
        garbage (map rand (range 1 10))
        salt   (apply str time garbage)]
    (secure-digest salt)))

(defn encrypt-password [password salt] 
  (reduce (fn [result _]
            (secure-digest result salt password *digest-site-key*))
          *digest-site-key*
          (range DIGEST-NUM-STRETCHES)))
