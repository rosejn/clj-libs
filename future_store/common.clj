;;  Copyright (c) Jeff Rose. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  rosejn (gmail)
;;  Created 29 July 2009

(ns future-store.common
  (:use 
     jlog
     clojure.contrib.seq-utils 
     (future-store graph)))

(defmethod get-properties :future-store.graph/graph [obj]
  (check-tx
    (doall 
      (map (fn [key]
             [key (get-property obj key)])
           (get-property-keys obj)))))

(defmethod property-count :future-store.graph/graph [obj]
  (count (get-property-keys obj)))

(defmethod in-nodes :future-store.graph/graph [n & labels]
  ;(info "(in-nodes " (get-id n) " " label ")")
  (check-tx
    (map #(edge-src %) (apply in-edges n labels))))

(defmethod out-nodes :future-store.graph/graph [n & labels]
  ;(info "(out-nodes " (get-id n) " " label ")")
  (check-tx
    (map #(edge-tgt %) (apply out-edges n labels))))

(defmethod link-new :future-store.graph/graph [src #^Keyword label & [props]]
  ;(info "(link-new " (get-id src) " " label " " props ")")
  (check-tx 
    (let [n (if props (add-node props) (add-node))]
      (add-edge src label n)
      n)))

(defmethod path-query :future-store.graph/graph [start path]
  (check-tx
    (if (empty? path)
      (do
        start)
      (let [label (keyword (first path))
                  children (out-nodes start label)
                  result (doall (map #(path-query % (rest path)) children))]
        (info "(path-query " (get-id start) " " path ")"
              " => " result)
        (flatten result)))))

(defmethod path-first :future-store.graph/graph [start path]
  (info "(path-first " (get-id start) " " path ")")
  (first (path-query start path)))

(defmethod out-degree :future-store.graph/graph [n]
  (count (out-edges n)))

(defmethod in-degree :future-store.graph/graph [n]
  (count (in-edges n)))

(defn- dfs-helper [node labels visitor visited]
    (if visitor
      (visitor node))

    (let [visited (assoc visited (get-id node) true)]
      (doseq [child (apply out-nodes node labels)]
        (if (not (contains? visited (get-id child)))
          (dfs-helper child labels visitor visited)))))

(defmethod dfs :future-store.graph/graph [start & args]
  (loop [node start
         labels (first (filter #(or (seq? %) (vector? %)) args))
         visitor (first (filter fn? args))
         visited {}]
    (dfs-helper start labels visitor visited)))

; These should return lazy sequences sitting on top of the java iterators
(defmethod all-nodes :future-store.graph/graph []
  (with-local-vars [nodes (list)]
    (dfs (root-node)
         (fn [node] (var-set nodes (conj (var-get nodes) node))))
    (var-get nodes)))

(defmethod all-edges :future-store.graph/graph []
  (apply concat (map out-edges (all-nodes))))

; These might have to actually count, or else we might need to keep track of
; the counts by hand if this is important, because I don't see a NEO method to
; get these values.
(defmethod node-count :future-store.graph/graph [])
(defmethod edge-count :future-store.graph/graph [])

