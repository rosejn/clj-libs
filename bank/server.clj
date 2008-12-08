(ns bank.client
  (:import (clojure.lang RT 
              Compiler Compiler$CompilerException 
              LispReader LineNumberingPushbackReader) 
           (java.io InputStreamReader OutputStreamWriter
                    PrintWriter)
           (java.net InetSocketAddress ServerSocket Socket SocketChannel)
           (java.lang.reflect Modifier Method Constructor)))

(defn deposit [who x] (rpc :deposit who x))

(defn withdraw [who x] (rpc :withdraw who x))

(defn balance [who] (rpc :balance who))

(def HOST "localhost")
(def PORT 2222)

(defn try-read
  [in]
  (let [s (new StringBuilder)]
    (loop [wait true]
      (if (or (.ready in) wait)
        (let [c (.read in)]
          (if (neg? c)
            (if wait
              nil
              (str s))
            (do
              (.append s (char c))
              (recur false))))
        (str s)))))

(defn rpc [fun & args]
  (let [sock (new Socket HOST PORT)
        out-stream (new OutputStreamWriter (.getOutputStream sock) RT/UTF8)
        in-stream (new InputStreamReader (.getInputStream sock) RT/UTF8)]
    (if (.isConnected sock)
      (do 
        (binding [*print-dup* true
                  *out* out-stream]
          (print (conj args fun)))
        (await-reply in-stream))
      (throw (new Exception "Error connecting to server!")))))

(defn await-reply [in-stream]
  (let [input (try-read in-stream)]
    (first input)))

