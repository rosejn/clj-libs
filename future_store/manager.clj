(ns future-store.manager
  (:import (java.util.concurrent 
             Executors 
             Callable 
             LinkedBlockingDeque
             FutureTask))
  (:use future-store jlog))


(def MANAGER-THREAD (ref nil))
(def STORE-NAME     (ref nil))
(def WORK-Q         (ref nil))
(def RUNNING        (ref false))

(defn- job-processor []
  (with-store @STORE-NAME
    (while @RUNNING
      (let [future (.takeFirst @WORK-Q)]
        (info "processing job...")
        (.run future)))))

(defn manager-start 
"Start the storage manager to provide shared, single threaded access to the DB."
  [store-name]
  (dosync 
      (ref-set WORK-Q (new LinkedBlockingDeque))
      (ref-set STORE-NAME store-name)
      (ref-set RUNNING true)
      (ref-set MANAGER-THREAD (new Thread job-processor)))
  (info "(manager-start " store-name ")")
  (.start @MANAGER-THREAD))

(defn manager-stop 
"Stop the storage manager, and optionally clear the pending job queue." 
  [& [clear-jobs]]
  (dosync (ref-set RUNNING false))
  (if clear-jobs (.clear @WORK-Q)))

(defn manager-do 
"Run the given Callable job in the manager thread.  By default this is a synchronous operation that will block until the job has completed, but if async is true a Future will be returned immediately representing the value."  
  [job & [async]]
  (let [future (new FutureTask job)]
    (.addLast @WORK-Q future)
    (info "manager-do got a job...")
    (if async
      future
      (.get future))))
