(ns config
  (:use clojure.contrib.duck-streams)
  (:import (java.io FileOutputStream FileInputStream)))

(defonce *storage* :frog-file)

(defonce CONFIG  (ref {}))
(defonce PERSIST (ref false))

(defn storage [& args] *storage*)

; Storage interface:
(defmulti save-config    storage)
(defmulti restore-config storage)

(defmacro with-file-lock [file & body]
  `(let [channel# (.getChannel ~file)
         lock#    (.lock channel#)]
     (try ~@body
       (finally 
         (if (.isValid lock#)
           (.release lock#))))))

(defn frog-name [name]
  (str name ".frog"))

; Simple file based storage w/ spit and slurp
(defmethod save-config :frog-file
  [path data]
  (with-open [file (FileOutputStream. (frog-name path))]
    (with-file-lock file
      (spit file data))))

; TODO: Can't lock a read-only channel, and the RandomAccessFile doesn't work with slurp*.  Ideally we make readers get a lock also though.
(defmethod restore-config :frog-file
  [path]
  (with-open [file (FileInputStream. (frog-name path))]
    ;;(with-file-lock file
      (read-string (slurp* file))))

(defn save [name]
  (save-config name @CONFIG))

(defn restore [name]
  (dosync
    (ref-set CONFIG (restore-config name))))

(defn with-store [name]
  (dosync (ref-set PERSIST name))
  (restore name))

(defn value
  ([key] (get @CONFIG key))
  ([key value] 
   (dosync (alter CONFIG assoc key value))
   (if @PERSIST
     (save @PERSIST))
   value))

(defn set-all [map]
  (dosync (ref-set CONFIG map)))

(defn defaults [map]
  (set-all map))

(defn get-all [] @CONFIG)

; Tests
(comment
(ns config.test
  (:use clojure.contrib.test-is)
  (:require config))

(deftest test-basic [] 
         (config/set-all {:a 1 :b 2})
         (is (= 1 (config/value :a)))

         (config/value :foo "asdf")
         (is (= "asdf" (config/value :foo)))

         (config/defaults {:a 10 :b 20 :foo 30})
         (is (= 30 (config/value :foo))))

(defn delete-file [name]
  (.delete (java.io.File. name)))

(deftest test-persist []
         (config/set-all {:a 1 :b 2})
         (config/save "test")
         (config/value :a 10)
         (config/value :b 20)
         (is (= 10 (config/value :a)))

         (config/restore "test")
         (is (= 2 (config/value :b)))
         
         (delete-file "test"))
)
