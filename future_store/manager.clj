(ns future-store.manager
  (:import (java.util.concurrent 
             Callable 
             LinkedBlockingDeque
             FutureTask
             TimeUnit))
  (:use future-store.raw jlog clj-backtrace.repl))

(defmacro assoc! [map key value]
  `(dosync (ref-set ~map (assoc (deref ~map) ~key ~value))))

(defmacro rset! [ref value]
  `(dosync (ref-set ~ref ~value)))

(def MANAGER (ref nil))

(def POLL-CYCLE 10) ; in ms

(defn- job-processor []
  (with-store (:store-path @MANAGER)
    (info "(job-processor " (:store-path @MANAGER) ") => " future-store.raw/*store*)
    (try 
      (while (:running @MANAGER)
             (let [job (.poll (:job-q @MANAGER) 10 TimeUnit/MILLISECONDS)]
               (if (not (nil? job))
                 (.run job))))
      (catch Exception e
        (println "Error processing a job in the store manager: " (pst-str e))))))

(defn manager-start 
"Start the storage manager to provide shared, single threaded access to the DB."
  [store-path]
  (rset! MANAGER { 
                 :running true
                 :thread (new Thread job-processor "FS-Manager Thread")
                 :store-path store-path
                 :job-q (new LinkedBlockingDeque)
                 })
  (info "(manager-start " store-path")")
  (.start (:thread @MANAGER)))

(defn manager-stop 
"Stop the storage manager, and clear the pending job queue." 
  []
  (assoc! MANAGER :running false)
  (.clear (:job-q @MANAGER))
  (.join (:thread @MANAGER))
  (rset! MANAGER nil))

(defn manager-do 
"Run the given Callable job in the manager thread.  By default this is a synchronous operation that will block until the job has completed, but if async is true then a Future representing the eventual return value will be returned immediately."  
  [job & [async]]
  (let [future (new FutureTask job)]
    (.addLast (:job-q @MANAGER) future)
    (info "manager-do got a job...")
    (if async
      future
      (.get future))))
