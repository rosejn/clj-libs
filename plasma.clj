(ns plasma.core
  (:use [clojure.contrib.javalog :only (log)])
  (:use net))

(def DEFAULT-PORT 5678)

(def graph-listener [db msg resp]
  (log :info "Listener got msg: " msg)
  (resp "response"))

(def listen []
  (add-log-file "global" "listener.log")
  (log :info "Starting listener...") 
  (net/listener DEFAULT-PORT (partial graph-listener db))

(def client []
  (add-log-file "global" "client.log")
  (log :info "Starting client...") 
  (plasma.net/start-client "localhost" DEFAULT-PORT))

(defn main []
  

(try 
  ;(log :info "command-line-args: " *command-line-args*)
  (main)
  (catch Exception e (.printStackTrace e)))

