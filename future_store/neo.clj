;;  Copyright (c) Jeff Rose. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  rosejn (gmail)
;;  Created 23 February 2009

(ns future-store.neo
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
  (:use 
     (future-store graph)
     clojure.set
     jlog))

(def BOTH Direction/BOTH)
(def INCOMING Direction/INCOMING)
(def OUTGOING Direction/OUTGOING)

(def BREADTH Traverser$Order/BREADTH_FIRST)
(def DEPTH   Traverser$Order/DEPTH_FIRST)

(def DEPTH-ONE StopEvaluator/DEPTH_ONE)
(def END-OF-GRAPH StopEvaluator/END_OF_GRAPH)

(def ALL ReturnableEvaluator/ALL)
(def ALL-BUT-START ReturnableEvaluator/ALL_BUT_START_NODE)

(defn open-store [path]
  (info "(open-store " path ")")
  (new EmbeddedNeo path))
;  (proxy [EmbeddedNeo clojure.lang.IMeta] [path]
;    (meta [] {:tag :future-store.graph/neo})))
    
(defn close-store [s]
  (info "(close-store)") 
  (.shutdown s))

(def *tx* nil)

(defn success [] (.success *tx*))
(defn failure [] (.failure *tx*))

(defmacro with-store [ #^String path & body ]
  `(binding [*store* (open-store ~path)]
     (try 
       (let [wg-result# (do ~@body)]
         wg-result#)
       (finally (close-store *store*)))))

(defmacro in-tx [& body]
  `(binding [*tx* (.beginTx *store*)]
     (try 
       (let [tx-result# (do ~@body)]
         tx-result#)
       (catch Exception e# 
         (do 
           (warning "Exception in transaction [" *tx* "]: " e#)
           (throw e#)))
       (finally (.finish *tx*)))))

;; Do the enclosing operations inside a transaction if they are not already
;; being performed in an outer transaction created by the user.
(defmacro check-tx [& body]
  `(do 
     (if (not *store*)
       (throw (Exception. "Trying to perform a storage operationg without defining the current store.")))
     (if *tx* 
       (do ~@body)
       (do (in-tx *store* 
                  (let [ctx-result# (do ~@body)]
                    (success)
                    ctx-result#))))))

(defmethod root-node :future-store.graph/neo [] 
  (check-tx (.getReferenceNode *store*)))

(defmethod find-node :future-store.graph/neo [id]
  (info "(find-node " id ")")
  (check-tx 
    (try 
      (.getNodeById *store* id)
      (catch NotFoundException e nil))))

(defmethod find-edge :future-store.graph/neo [id]
  (check-tx 
    (try 
      (.getRelationshipById *store* id)
      (catch NotFoundException e nil))))

(defmethod edge-label :future-store.graph/neo [e]
  (check-tx
    (.name (.getType e))))

(defmethod edge-src :future-store.graph/neo [e]
  (check-tx
    (.getStartNode e)))

(defmethod edge-tgt :future-store.graph/neo [e]
  (check-tx
    (.getEndNode e)))

(defmethod get-id :future-store.graph/neo [obj]
  (if (not (or (instance? Node obj)
               (instance? Relationship obj)))
    (throw 
      (IllegalArgumentException. 
        (str "Must pass either a Node or a Relationship (Edge) to get-id, got: " (class obj)))))
  (.getId obj))

(defmethod has-property? :future-store.graph/neo [obj key]
  (check-tx (.hasProperty obj (str key))))

(defmethod get-property :future-store.graph/neo [obj key]
  ;(info "(get-property " (get-id obj) " " key ")")
  (check-tx 
    (try 
      (.getProperty obj (str key))
      (catch org.neo4j.api.core.NotFoundException e
        nil))))

(defmethod get-property-keys :future-store.graph/neo [obj]
  ;(info "(get-property-keys " (get-id obj) ")")
  (check-tx 
    (seq (.getPropertyKeys obj))))

(defmethod set-property :future-store.graph/neo [obj key value]
  ;(info "(set-property " (get-id obj) " {" key " " value "})")
  (check-tx (.setProperty obj (str key) value))
  obj)

(defmethod add-node :future-store.graph/neo [& [props]]
  ;(info "(add-node " props ")")
  (check-tx 
    (let [n (.createNode *store*)]
      (doseq [[k v] props] (set-property n k v))
      n)))

(defmethod remove-node :future-store.graph/neo [#^Node n]
  ;(info "(remove-node " (get-id n) ")")
  (try 
    (check-tx 
      ; Remove all edges pointing to and from this node
      (doseq [edge (in-edges n)] (.delete edge))
      (doseq [edge (out-edges n)] (.delete edge))
      (.delete n))
    true
    (catch NotFoundException e false)))

(defmethod edge-type :future-store.graph/neo [#^Keyword n]
  (proxy [RelationshipType] [] (name [] (name n))))

(defmethod add-edge :future-store.graph/neo [#^Node src #^Keyword label #^Node dest & [props]]
  ;(info "(add-edge " (get-id src) " " label " " (get-id dest) " " props ")")
  (check-tx 
    (let [edge (.createRelationshipTo src dest (edge-type label))]
      (doseq [[k v] props] (set-property edge k v))
      edge)))

(def link add-edge)

(defmethod remove-edge :future-store.graph/neo [#^Relationship e]
  (check-tx 
    (.delete e)))

(defn- get-edges [#^Node n #^Direction direction labels]
  (check-tx 
    (let [edges (seq (.getRelationships n direction))
          label-filter (fn [edge] (some 
                                    #(= % (keyword (edge-label edge))) 
                                    labels))
          filtered (if (empty? labels)
                     edges
                     (doall (filter label-filter edges)))]
      ;(info "get-edges edges: " (count (seq edges)))
      ;(info "get-edges filtered: " (count (seq filtered)))
      ;(info "(get-edges " 
      ;      (get-id n) " " direction " " labels ")"
      ;      " => " (count (seq edges)) " edges")
      filtered)))

(defmethod in-edges :future-store.graph/neo [#^Node n & labels]
  (get-edges n INCOMING labels))

(defmethod out-edges :future-store.graph/neo [#^Node n & labels]
  (get-edges n OUTGOING labels))

(defn jcall [obj name & args]
  (clojure.lang.Reflector/invokeInstanceMethod obj (str name)
    (if args (to-array args) clojure.lang.RT/EMPTY_ARRAY)))

(defn jfn [name]
  #(apply jcall %1 name %&))

;((jfn 'substring) "fred" 2 3)
;((jfn 'toUpperCase) "fred") 

; TODO: Figure out how to do these without these annoying 
; relationship type-constraints on which edges we can traverse...
; - maybe we can use custom iterator functions?
(defn ndfs [#^Node start labels]
  (let [rel-types (doall (mapcat (fn [label] [(edge-type label) OUTGOING]) labels))]
    (seq (.iterator (apply (jfn 'traverse) start DEPTH END-OF-GRAPH 
                               ALL rel-types)))))

(defmethod bfs :future-store.graph/neo [#^Node start]
  (seq (.iterator (.traverse start BREADTH END-OF-GRAPH 
                             ALL (edge-type :link) OUTGOING))))
