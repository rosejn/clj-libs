(ns future-store.index
  (:import (org.neo4j.util.index SingleValueIndex))
  (:use future-store))

;; TODO: The index component is not thread safe yet, so this might need to be
;; guarded by an agent or something...

(defn index-property [node key & [label]]
  (check-tx 
    (let [idx-name (str (get-id node) "-[" label "]-" key)
          idx-node (link-new node :index {:index-name idx-name,
                                          :index-edge-label (str label),
                                          :index-prop-key (str key)})
          index (new SingleValueIndex idx-name idx-node *store*)
          children (out-nodes node label)]
      (doseq [child children]
        (.index index child (get-property child key)))
      index)))

(defn lookup [index value]
  (.getSingleNodeFor index value))

;; Tests and time trials follow
(use 'clojure.contrib.test-is)

(defn child-by-property [parent label property value]
  (let [children (out-nodes parent label)]
    (first (filter #(= value (get-property % property)) children))))

(deftest index-vs-raw []
  (with-store "index-test-db"
    (let [root (root-node)
          size 100]
      (dotimes [i size]
        (link-new root :foo {:counter i}))
      (let [index (index-property root :counter :foo)
            num-trials 10
            trial-seq (for [i (range num-trials)] (rand-int size))]
        (doseq [val trial-seq]
          (let [ival (lookup index val)
                rval (child-by-property root :foo :counter val)]
            (info "val: " val " ival: " (get-property ival :counter) 
                  " rval: " (get-property rval :counter))
            (is (= ival rval)))))))
  (delete-graph "test-db"))

(defn fs-index-test [] (run-tests (find-ns 'future-store.index)))

(def index-speed-test []
  (with-store "index-speed-db"
    (let [root (root-node)
          size 1000]
      (dotimes [i size] (link-new root :foo {:counter i}))

      (let [index (index-property root :counter :foo)
            num-trials 100
            trial-seq (for [i (range num-trials)] (rand-int size))
            raw-time (time (doseq [val trial-seq]
                             (child-by-property root :foo :counter val)))
            index-time (time (doseq [val trial-seq]
                               (lookup index val)))]
        (println "Raw time: " raw-time "\nIndex time: " index=-time))))
  (delete-graph "test-db"))
