(ns bank.server
  (:import (clojure.lang RT 
              Compiler Compiler$CompilerException 
              LispReader LineNumberingPushbackReader) 
           (java.io InputStreamReader OutputStreamWriter
                    PrintWriter)
;           (java.net InetAddress ServerSocket Socket SocketChannel)
           (java.lang.reflect Modifier Method Constructor))
  (:use graph net
        [clojure.contrib.javalog :only (log)]))

(def LISTENER-PORT 4242)

(defn msg-handler [db msg resp] 
  (log :info "echoing msg: " msg)
  (resp msg))

(defn start []
  (let [db  (graph :neo "account-database")
        srv (net/listener LISTENER-PORT (partial msg-handler db))]
    srv))
