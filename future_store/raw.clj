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

(ns future-store.raw
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
  (:import (java.io File))
  (:use 
     clojure.contrib.seq-utils 
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

(defn close-store [s]
  (info "(close-store)") 
  (.shutdown s))

(declare delete-dir)

(defn delete-files [file-list]
  (if (not (empty? file-list))
    (let [f (first file-list)]
      (if (.isDirectory f) (delete-dir f) (.delete f))
      (recur (rest file-list)))))

(defn delete-dir [dir]
  (if (.exists dir)
    (do 
      (let [files (.listFiles dir)]
        (delete-files files))
      (.delete dir))))

(defn delete-store [path]
  (info "(delete-store " path ")")
  (let [dir (new File path)]
    (delete-dir dir)))

(def *store* nil)
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

(defn root-node [] 
  (check-tx (.getReferenceNode *store*)))

(defn find-node [id]
  (info "(find-node " id ")")
  (check-tx 
    (try 
      (.getNodeById *store* id)
      (catch NotFoundException e nil))))

(defn find-edge [id]
  (check-tx 
    (try 
      (.getRelationshipById *store* id)
      (catch NotFoundException e nil))))

(defn edge-label [e]
  (check-tx
    (.name (.getType e))))

(defn edge-src [e]
  (check-tx
    (.getStartNode e)))

(defn edge-tgt [e]
  (check-tx
    (.getEndNode e)))

(defn get-id [obj]
  (if (not (or (instance? Node obj)
               (instance? Relationship obj)))
    (throw 
      (IllegalArgumentException. 
        (str "Must pass either a Node or a Relationship (Edge) to get-id, got: " (class obj)))))
  (.getId obj))

(defn has-property? [obj key]
  (check-tx (.hasProperty obj (str key))))

(defn get-property [obj key]
  (info "(get-property " (get-id obj) " " key ")")
  (check-tx (.getProperty obj (str key))))

(defn get-property-keys [obj]
  (info "(get-property-keys " (get-id obj) ")")
  (check-tx 
    (seq (.getPropertyKeys obj))))

(defn get-properties [obj]
  (check-tx
    (doall 
      (map (fn [key]
             [key (get-property obj key)])
           (get-property-keys obj)))))

(defn property-count [obj]
  (count (get-property-keys obj)))

(defn set-property [obj key value]
  (info "(set-property " (get-id obj) " {" key " " value "})")
  (check-tx (.setProperty obj (str key) value))
  obj)

(defn wrap-entry [k v]
  (proxy [clojure.lang.IMapEntry] []
    (key [] k)
    (val [] v)))

(defn wrap-assoc [obj]
  (proxy [clojure.lang.Associative] []
    (count [] (property-count obj))
    (seq   [] (get-properties obj))
    (cons  [[k v]] (set-property obj k v))
    (empty [] {}) ; Not sure what would make sense here...
    (equiv [o] (and
                 (= (class o) (class obj))
                 (= (.getId o) (.getId obj))))
    (containsKey [k] (has-property? obj k))
    (entryAt     [k] (wrap-entry k (get-property obj k)))
    (assoc       [k v] (do (set-property obj k v) (wrap-assoc obj)))
    (valAt       ([k] (get-property obj k))
                 ([k d] (if (has-property? obj k) 
                          (get-property obj k)
                          d)))))

; These should return lazy sequences sitting on top of the java iterators
(defn all-nodes [])
(defn all-edges [])

; These might have to actually count, or else we might need to keep track of
; the counts by hand if this is important, because I don't see a NEO method to
; get these values.
(defn node-count [])
(defn edge-count [])

(defn add-node [& [props]]
  (info "(add-node " props ")")
  (check-tx 
    (let [n (.createNode *store*)]
      (doseq [[k v] props] (set-property n k v))
      n)))

(declare in-edges out-edges)

(defn remove-node [#^Node n]
  (info "(remove-node " (get-id n) ")")
  (try 
    (check-tx 
      ; Remove all edges pointing to and from this node
      (doseq [edge (in-edges n)] (.delete edge))
      (doseq [edge (out-edges n)] (.delete edge))
      (.delete n))
    true
    (catch NotFoundException e false)))

(defn edge-type [#^Keyword n]
  (proxy [RelationshipType] [] (name [] (name n))))

(defn add-edge [#^Node src #^Keyword label #^Node dest & [props]]
  (info "(add-edge " (get-id src) " " label " " (get-id dest) " " props ")")
  (check-tx 
    (let [edge (.createRelationshipTo src dest (edge-type label))]
      (doseq [[k v] props] (set-property edge k v))
      edge)))

(def link add-edge)

(defn link-new [#^Node src #^Keyword label & [props]]
  (info "(link-new " (get-id src) " " label " " props ")")
  (check-tx 
    (let [n (if props (add-node props) (add-node))]
      (add-edge src label n)
      n)))

(defn remove-edge [#^Relationship e]
  (check-tx 
    (.delete e)))

(defn- edge-filter [label]
  ;(info "(edge-filter " label ")")
  (fn [edge] (= label (keyword (edge-label edge)))))

(defn- get-edges [#^Node n #^Direction direction label]
  (check-tx 
    (let [edges (.getRelationships n direction)
                filtered (if label
                           (doall (filter (edge-filter label) (seq edges)))
                           (seq edges))]
      ;(info "get-edges edges: " (count (seq edges)))
      ;(info "get-edges filtered: " (count (seq filtered)))
      (info "(get-edges " 
            (get-id n) " " direction " " label ")"
            " => " (count (seq edges)) " edges")
      filtered)))

(defn in-edges [#^Node n & [label]]
  (get-edges n INCOMING label))

(defn out-edges [#^Node n & [label]]
  (let [edges (get-edges n OUTGOING label)]
    edges))

(defn in-nodes [#^Node n & [label]]
  ;(info "(in-nodes " (get-id n) " " label ")")
  (check-tx
    (map #(.getStartNode %) (in-edges n label))))

(defn out-nodes [#^Node n & [label]]
  ;(info "(out-nodes " (get-id n) " " label ")")
  (check-tx
    (map #(.getEndNode %) (out-edges n label))))

(defn path-query [#^Node start path]
  (check-tx
    (if (empty? path)
      (do
        start)
      (let [label (keyword (first path))
                  children (out-nodes start label)
                  result (map #(path-query % (rest path)) children)]
        (info "(path-query " (get-id start) " " path ")"
              " => " result)
        (flatten result)))))

(defn path-first [#^Node start path]
  (info "(path-first " (get-id start) " " path ")")
  (first (path-query start path)))

(defn out-degree [#^Node n]
  (count (out-edges n)))

(defn in-degree [#^Node n]
  (count (in-edges n)))

(defn dfs [#^Node start visitor]
  (map visitor 
       (seq (.iterator (.traverse start DEPTH END-OF-GRAPH 
                                  ALL :link OUTGOING)))))

(defn bfs [#^Node start visitor]
  (map visitor 
       (seq (.iterator (.traverse start BREADTH END-OF-GRAPH 
                                  ALL :link OUTGOING)))))
