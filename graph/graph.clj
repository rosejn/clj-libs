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
  (:use [clojure.contrib.fcase :only (case)
         clojure.contrib.str-utils :only (str-join)]))

(def graph-stores (ref {}))

(defn register-graph-store 
  "Add a back-end graph store to the set of available graph types."
  [key store-fn] 
    (dosync (ref-set graph-stores (assoc @graph-stores key store-fn))))

(defn graph 
  ([] (graph :hlist))
  ([store-key & args] (apply (store-key @graph-stores) args)))

(defmulti get-node    :graph-store)
(defmulti set-root    :graph-store)

(defmulti graph-id    :graph-store)
(defmulti node-id     :graph-store)
(defmulti edge-id     :graph-store)

(defmulti get-node    :graph-store)
(defmulti get-edge    :graph-store)

(defmulti nodes       :graph-store)
(defmulti edges       :graph-store)

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

(defn to_dot [g]
  (str-join "\n" 
  (print "digraph" (graph-id g) "{")
    (map (nodes g)

(defmacro with-tx [& body]
  `(let [tx (new Transaction)]
     (try 
       (do 
         (.begin tx)
         ~@body )
       (catch Exception e 
         (do 
           (.printStackTrace e)
           (throw e)))
       (finally (.finish tx)))))

(use 'graph.hlist)
(use 'graph.neo)

(defn reload []
  (require '(graph hlist neo) :reload-all))

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
  ;(let [added (add-n 100 (graph :neo "db"))
  (let [added (add-n 100 (graph))
        nc1 (node-count added)
        removed (remove-n 50 added)
        nc2 (node-count removed)]
    (is (= 50 (- nc1 nc2)))))

(run-tests)
