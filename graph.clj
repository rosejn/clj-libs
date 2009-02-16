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

(ns graph
  (:use [clojure.contrib.str-utils :only [str-join]]))

(def graph-stores (ref {}))

(defn register-graph-store 
  "Add a back-end graph store to the set of available graph types."
  [key store-fn] 
  (dosync (ref-set graph-stores (assoc @graph-stores key store-fn))))

(defn graph 
  ([] (graph :neo))
  ([store-key & args] (apply (store-key @graph-stores) args)))

(defmulti get-root    :graph-store)
(defmulti set-root    :graph-store)

(defmulti graph-id    :graph-store)
(defmulti node-id     :graph-store)
(defmulti edge-id     :graph-store)

(defmulti get-node    :graph-store)
(defmulti get-edge    :graph-store)

(defmulti all-nodes   :graph-store)
(defmulti all-edges   :graph-store)

(defmulti node-count  :graph-store)
(defmulti edge-count  :graph-store)

(defmulti add-node    :graph-store)
(defmulti remove-node :graph-store)

(defmulti add-edge    :graph-store)
(defmulti remove-edge :graph-store)

(defmulti in-nodes    :graph-store)
(defmulti out-nodes   :graph-store)

(defmulti in-edges    :graph-store)
(defmulti out-edges   :graph-store)

(defmulti path-query  :graph-store)

(defmulti dfs         :graph-store)
(defmulti bfs         :graph-store)

(defn to_dot [g]
  (str-join "\n" 
            (pr-str "digraph" (graph-id g) "{")
            ;(map (nodes g))))
            (pr-str "}")))

;(use 'graph.hlist)
(use 'graph.neo)

(defn reload []
;  (require '(graph hlist neo) :reload-all))
  (require '(graph.neo) :reload-all))

(comment
(deftest add-remove-hlist []
         (let [added (add-n 100 (graph))
               nc1 (node-count added)
               removed (remove-n 50 added)
               nc2 (node-count removed)]
           (is (= 50 (- nc1 nc2))))))
  
;; Tests follow
(use 'clojure.contrib.test-is)

(defn add-n [g n]
  (if (zero? n)
    g
    (recur (add-node g {:val n}) (dec n))))

(defn remove-n [g n]
  (let [ids (take n (all-nodes g))]
    (loop [graph g
           uuids ids]
      (if (empty? uuids)
        graph
        (recur (remove-node graph (first uuids))
               (rest uuids))))))

(deftest add-remove-neo []
  (neo4j/tx 
    (let [g (graph :neo "db")]
      (neo4j/success))))

;          added (add-n g 100)
;          nc1 (node-count added)
;          removed (remove-n g 50)
;          nc2 (node-count removed)]
;      (is (= 50 (- nc1 nc2))))))

(defn test-neo [] (run-tests (find-ns 'graph)))
