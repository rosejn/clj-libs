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

(defonce MANAGER (ref {:running false}))

(defn running? []
  (:running @MANAGER))

(def POLL-CYCLE 10) ; in ms

(defn- next-job []
  (let [job 
        (.take (:job-q @MANAGER))
        ]
    (info "$$$$$$$$$$ next-job: " job)
    job))

(defn- job-processor []
  (with-store (:store-path @MANAGER)
    (info "(job-processor " (:store-path @MANAGER) ") => " future-store.raw/*store*)
    (try 
      (while (running?) 
        (.run (next-job)))
      (catch InterruptedException e nil)
      (catch Exception e
        (println "Error processing a job in the store manager: " (pst-str e)))))
  (info "job processor completed: " *store*))

(defn jobs-pending []
  (.size (:job-q @MANAGER)))

(defn alive? [thread]
  (.isAlive thread))

(defn stop 
"Stop the storage manager, and clear the pending job queue." 
  []
  (info "(manager-stop) running: " (running?))
  (if (running?)
    (do 
      (assoc! MANAGER :running false)
      (info "manager-stop 2: " @MANAGER)
      (.clear (:job-q @MANAGER))
      (.interrupt (:thread @MANAGER))
      (.join (:thread @MANAGER))
      (info "manager-stop 3: " (alive? (:thread @MANAGER))))))

(defn start 
"Start the storage manager to provide shared, single threaded access to the DB."
  [store-path]
  (when (not (and (= store-path (:store-path @MANAGER))
                (running?)))
    (stop)
    (rset! MANAGER { 
                    :running true
                    :thread (new Thread job-processor "FS-Manager Thread")
                    :store-path store-path
                    :job-q (new LinkedBlockingDeque)
                    })
    (info "(manager-start " store-path")")
    (.start (:thread @MANAGER))))

(defn do 
"Run the given Callable job in the manager thread.  By default this is a synchronous operation that will block until the job has completed, but if async is true then a Future representing the eventual return value will be returned immediately."  
  [job & [async]]
  (let [future (new FutureTask job)]
    (.addLast (:job-q @MANAGER) future)
    (info "manager-do got a job...")
    (if async
      future
      (.get future))))
