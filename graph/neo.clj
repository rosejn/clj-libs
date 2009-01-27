;;  Copyright (c) Jeff Rose. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  neo.clj
;;
;;  A neo4j back-end to the graph library.
;;
;;  rosejn (gmail)
;;  Created 15 November 2008

(ns graph.neo
  (:import (org.neo4j.api.core EmbeddedNeo))
  (:use neo4j)
  (:use clojure.contrib.seq-utils))
  
(refer 'graph)
(refer 'neo4j)

(defn neo-graph [& [data-dir]]
  (let [data-dir (or data-dir "graph-data")]
    (new EmbeddedNeo data-dir)))

(register-graph-store :neo neo-graph)

(defmethod get-root :neo [g] 
  (. g getReferenceNode))

(defmethod get-node :neo [g id]
  (. g getNodeById id))

(defmethod get-edge :neo [g id]
  (. g getRelationshipById id))

; These should return lazy sequences sitting on top of the java iterators
(defmethod all-nodes :neo [g])
(defmethod all-edges :neo [g])

; These might have to actually count, or else we might need to keep track of
; the counts by hand if this is important, because I don't see a NEO method to
; get these values.
(defmethod node-count :neo [g])
(defmethod edge-count :neo [g])

(defmethod add-node :neo [g props]
  (let [n (new-node)]
    (map (fn [[k v]] (.setProperty n k v)) props)
    n))

(defmethod remove-node :neo [g n]
  (.delete g n))

(defmethod add-edge :neo [g src label dest & [props]]
  (let [edge (relate src label dest)]
    (map (fn [[k v]] (.setProperty edge k v)) props)))

(defmethod remove-edge :neo [g e]
  (.delete e))

(defmethod in-edges :neo [g n]
  (seq (.getRelationships n neo4j/incoming)))

(defmethod out-edges :neo [g n]
  (seq (.getRelationships n neo4j/outgoing)))

(defmethod in-nodes :neo [g n]
  (map (fn [edge] (.getStartNode edge))
       (in-edges g n)))

(defmethod out-nodes :neo [g n]
  (map (fn [edge] (.getEndNode edge))
       (out-edges g n)))

(defmethod path-query :neo [g start path]
  (if (empty? path)
    start
    (let [label (first path)
          children (out-nodes g start)]
      (map (fn [child] (path-query child (rest path))) children))))

(defmethod dfs :neo [g start visitor]
  (map visitor 
       (seq (.iterator (.traverse start neo4j/depth neo4j/end-of-graph 
                                  neo4j/all :link neo4j/outgoing)))))

(defmethod bfs :neo [g start visitor]
  (map visitor 
       (seq (.iterator (.traverse start neo4j/breadth neo4j/end-of-graph 
                                  neo4j/all :link neo4j/outgoing)))))

(defn close [g]
  (. g shutdown))

