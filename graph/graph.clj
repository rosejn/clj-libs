;;  Copyright (c) Jeff Rose. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  graph.clj
;;
;;  A Clojure graph library
;;
;;  rosejn (gmail)
;;  Created 15 November 2008

(ns graph)

(def default-graph-store (ref nil))

(defn uuid 
  "Creates a random, immutable UUID object that is comparable using the '=' function."
  [] (. java.util.UUID randomUUID))

(defn graph 
  ([] (@default-graph-store))
  ([graph-store] (graph-store)))

(defmulti get-root :graph-store)
(defmulti get-node :graph-store)
(defmulti get-edge :graph-store)

(defmulti nodes :graph-store)
(defmulti edges :graph-store)

(defmulti node-count :graph-store)
(defmulti edge-count :graph-store)

(defmulti add-node :graph-store)
(defmulti remove-node :graph-store)

(defmulti add-edge :graph-store)
(defmulti remove-edge :graph-store)

(defmulti in-nodes :graph-store)
(defmulti out-nodes :graph-store)

(defmulti in-edges :graph-store)
(defmulti out-edges :graph-store)

(load-file "hlist.clj")
(dosync (ref-set default-graph-store hlist-graph))

;; Tests follow
(use 'clojure.contrib.test-is)

(defn add-n [n g]
  (if (zero? n)
    g
    (recur (dec n) (add-node g {:val n}))))

(defn remove-n [n g]
  (let [ids (take n (nodes g))]
    (loop [graph g
           uuids ids]
      (if (empty? uuids)
        graph
        (recur (remove-node graph (first uuids))
               (rest uuids))))))

(deftest add-remove []
  (let [added (add-n 100 (graph))
        nc1 (node-count added)
        removed (remove-n 50 added)
        nc2 (node-count removed)]
    (is (= 50 (- nc1 nc2)))))

(run-tests)
