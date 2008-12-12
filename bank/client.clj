(ns bank.client
  (:import (clojure.lang RT 
              Compiler Compiler$CompilerException 
              LispReader LineNumberingPushbackReader) 
           (java.io InputStreamReader OutputStreamWriter
                    PrintWriter)
;           (java.net InetSocketAddress ServerSocket Socket SocketChannel)
           (java.lang.reflect Modifier Method Constructor))
  (:use graph net
        [clojure.contrib.javalog :only (log)]))

(comment 
(defn deposit [who x] (rpc :deposit who x))

(defn withdraw [who x] (rpc :withdraw who x))

(defn balance [who] (rpc :balance who))
)

(def HOST "localhost")
(def PORT 4242)

(defn msg-handler [msg resp]
  (log :info "Client got msg: " msg))

(defn connect-handler [resp]
  (resp "This is a test msg"))

(comment
  (resp {:a "This is a test message!",
         :b 23423423
         :c [1 2 3]}))

(defn start []
  (let [handlers {:on-msg msg-handler, :on-connect connect-handler}
        cln (net/client HOST PORT handlers)]
    cln))

(comment 
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
  (wait-reply in-stream))
  (throw (new Exception "Error connecting to server!")))))

  (defn wait-reply [in-stream]
  (let [input (try-read in-stream)]
  (first input)))

  )
