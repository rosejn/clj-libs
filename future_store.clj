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

(defmacro in-tx [g & body]
  `(binding [*tx* (.beginTx ~g)]
     (try ~@body
       (finally (.shutdown ~g)))))

(defn done [] (.success *tx*))

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

(defn add-node [g & props]
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

(defn remove-edge [g e]
  (.delete e))

(defn in-edges [g n]
  (seq (.getRelationships n INCOMING)))

(defn out-edges [g n]
  (seq (.getRelationships n OUTGOING)))

(defn in-nodes [g n]
  (map (fn [edge] (.getStartNode edge))
       (in-edges g n)))

(defn out-nodes [g n]
  (map (fn [edge] (.getEndNode edge))
       (out-edges g n)))

(defn path-query [g start path]
  (if (empty? path)
    start
    (let [label (first path)
          children (out-nodes g start)]
      (map (fn [child] (path-query child (rest path))) children))))

(defn dfs [g start visitor]
  (map visitor 
       (seq (.iterator (.traverse start DEPTH END-OF-GRAPH 
                                  ALL :link OUTGOING)))))

(defn bfs [g start visitor]
  (map visitor 
       (seq (.iterator (.traverse start BREADTH END-OF-GRAPH 
                                  ALL :link OUTGOING)))))

