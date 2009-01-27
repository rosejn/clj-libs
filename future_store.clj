;;  Copyright (c) Jeff Rose. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  future-store.clj 
;;
;;  A neo4j based graph storage library. 
;;
;;  Note: Bits of neo4j oriented code taken from
;;  the neo4j-clojure library...  Thanks!
;;
;;  rosejn (gmail)
;;  Created 15 November 2008

(ns future-store
  (:import (org.neo4j.api.core Direction
                               EmbeddedNeo
                               NeoService
                               Node
                               NotFoundException
                               NotInTransactionException
                               PropertyContainer
                               Relationship
                               RelationshipType
                               ReturnableEvaluator
                               StopEvaluator
                               Transaction
                               TraversalPosition
                               Traverser
                               Traverser$Order))
  (:use clojure.contrib.seq-utils))
  
(def BOTH Direction/BOTH)
(def INCOMING Direction/INCOMING)
(def OUTGOING Direction/OUTGOING)

(def BREADTH Traverser$Order/BREADTH_FIRST)
(def DEPTH   Traverser$Order/DEPTH_FIRST)

(def DEPTH-ONE StopEvaluator/DEPTH_ONE)
(def END-OF-GRAPH StopEvaluator/END_OF_GRAPH)

(def ALL ReturnableEvaluator/ALL)
(def ALL-BUT-START ReturnableEvaluator/ALL_BUT_START_NODE)

(defn open-graph [path]
  (new EmbeddedNeo path))

(defn close-graph [g]
  (.shutdown g))

(def *tx* nil)

(defn success [] (.success *tx*))

(defmacro in-tx [g & body]
  `(binding [*tx* (.beginTx ~g)]
     (try ~@body
       (catch Exception e# 
         (println "Exception in transaction: " e#)
         (throw e#))
       (finally (.finish *tx*)))))

(defn get-root [g] 
  (.getReferenceNode g))

(defn get-node [g id]
  (.getNodeById g id))

(defn get-edge [g id]
  (.getRelationshipById g id))

; These should return lazy sequences sitting on top of the java iterators
(defn all-nodes [g])
(defn all-edges [g])

; These might have to actually count, or else we might need to keep track of
; the counts by hand if this is important, because I don't see a NEO method to
; get these values.
(defn node-count [g])
(defn edge-count [g])

(defn add-node [g & [props]]
  (let [n (.createNode g)]
    (map (fn [[k v]] (.setProperty n k v)) props)
    n))

(defn remove-node [g n]
  (.delete g n))

(defn- edge-type [#^Keyword n]
  (proxy [RelationshipType] []
    (name [] (name n))))

(defn add-edge [#^Node src #^Keyword label #^Node dest & [props]]
  (let [edge (.createRelationshipTo src dest (edge-type label))]
    (map (fn [[k v]] (.setProperty edge k v)) props)))

(defn link-new [g src label & [props]]
  (let [n (if props (add-node g props) (add-node g))]
    (add-edge src label n)
    n))

(defn remove-edge [e]
  (.delete e))

(defn- edge-filter [label]
  (println "filtering with label: " label)
  (fn [edge] (= label (keyword (.name (.getType edge))))))

(defn- get-edges [n direction label]
  (println "get-edges *tx*: " *tx*)
  (let [edges (seq (.getRelationships n direction))]
  (println "get-edges2 *tx*: " *tx*)
    (if label
      (filter (edge-filter label) edges)
      edges)))

(defn in-edges [n & [label]]
  (get-edges n INCOMING label))

(defn out-edges [n & [label]]
  (println "out-edges label: " label)
  (get-edges n OUTGOING label))

(defn in-nodes [n & [label]]
  (map (fn [edge] (.getStartNode edge))
       (in-edges n label)))

(defn out-nodes [n & [label]]
  (println "out-nodes label: " label)
  (map (fn [edge] (.getEndNode edge))
       (out-edges n label)))

(defn path-query [start path]
  (println "path: " path "\n*tx*: " *tx*)
  (if (empty? path)
    start
    (let [label (first path)
          children (out-nodes start label)]
      (map (fn [child] (path-query child (rest path))) children))))

(defn dfs [start visitor]
  (map visitor 
       (seq (.iterator (.traverse start DEPTH END-OF-GRAPH 
                                  ALL :link OUTGOING)))))

(defn bfs [start visitor]
  (map visitor 
       (seq (.iterator (.traverse start BREADTH END-OF-GRAPH 
                                  ALL :link OUTGOING)))))

