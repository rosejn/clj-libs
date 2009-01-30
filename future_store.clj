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

(use 'clj-backtrace.repl)

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
  (:import (java.util.logging Logger Level))
  (:import (java.io File))
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

(def FS-LOG (Logger/getLogger "future-store-log"))
(defn info    [& msg] (.info FS-LOG (apply str msg)))
(defn warning [& msg] (.warning FS-LOG (apply str msg)))
(defn severe  [& msg] (.severe FS-LOG (apply str msg)))

(defn open-graph [path]
  (info "(open-graph " path ")")
  (new EmbeddedNeo path))

(defn close-graph [g]
  (info "(close-graph)") 
  (.shutdown g))

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

(defn delete-graph [path]
  (info "(delete-graph " path ")")
  (let [dir (new File path)]
    (delete-dir dir)))

(def *g* nil)
(def *tx* nil)

(defn success [] (.success *tx*))
(defn failure [] (.failure *tx*))

(defmacro with-graph [ #^String path & body ]
  `(binding [*g* (open-graph ~path)]
    (try ~@body
      (finally (close-graph *g*)))))

(defmacro in-tx [& body]
  `(binding [*tx* (.beginTx *g*)]
     (try ~@body
       (catch Exception e# 
         (println "Exception in transaction: " e#)
         (throw e#))
       (finally (.finish *tx*)))))

;; Do the enclosing operations inside a transaction if they are not already
;; being performed in an outer transaction created by the user.
(defmacro check-tx [& body]
  `(if *tx* 
     (do ~@body)
     (do (in-tx *g* 
                (let [result# ~@body]
                  (success)
                  result#)))))

(defn get-root [] 
  (check-tx (.getReferenceNode *g*)))

(defn get-node [id]
  (check-tx (.getNodeById *g* id)))

(defn get-edge [id]
  (check-tx (.getRelationshipById *g* id)))

(defn get-id [obj]
  (.getId obj))

(defn get-property [obj key]
  (info "(get-property " (get-id obj) key ")")
  (check-tx (.getProperty obj (str key))))

(defn get-properties [obj]
  (info "(get-properties " (get-id obj) ")")
  (check-tx 
    (seq (.getPropertyKeys obj))))

(defn set-property [obj key value]
  (info "(set-property " (get-id obj) "{" key value "})")
  (check-tx (.setProperty obj (str key) value)))

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
    (let [n (.createNode *g*)]
      (dorun (map (fn [[k v]] (set-property n k v)) props))
    n)))

(defn remove-node [n]
  (check-tx 
    (.delete *g* n)))

(defn- edge-type [#^Keyword n]
  (proxy [RelationshipType] []
    (name [] (name n))))

(defn add-edge [#^Node src #^Keyword label #^Node dest & [props]]
  (check-tx 
    (let [edge (.createRelationshipTo src dest (edge-type label))]
    (map (fn [[k v]] (set-property edge k v)) props))))

(defn link-new [src label & [props]]
  (check-tx 
    (let [n (if props (add-node props) (add-node))]
    (add-edge src label n)
    n)))

(defn delete-edge [e]
  (check-tx 
    (.delete e)))

(defn- edge-filter [label]
  (println "edge-filter " label ")")
  (fn [edge] (= label (keyword (.name (.getType edge))))))

(defn- get-edges [n direction label]
  (info "(get-edges " (get-id n) direction label ")")
  (check-tx (let [edges (seq (.getRelationships n direction))]
    (if label
      (filter (edge-filter label) edges)
      edges))))

(defn in-edges [n & [label]]
  (get-edges n INCOMING label))

(defn out-edges [n & [label]]
  (info "(out-edges " label ")")
  (get-edges n OUTGOING label))

(defn in-nodes [n & [label]]
  (map (fn [edge] (.getStartNode edge))
       (in-edges n label)))

(defn out-nodes [n & [label]]
  (info "(out-nodes " label ")")
  (map (fn [edge] (.getEndNode edge))
       (out-edges n label)))

(defn path-query [start path]
  (info "(path-query " (get-id start) path ")")
  (check-tx
  (if (empty? path)
    start
    (let [label (first path)
          children (out-nodes start label)]
      (dorun (map (fn [child] (path-query child (rest path))) children))))))

(defn dfs [start visitor]
  (map visitor 
       (seq (.iterator (.traverse start DEPTH END-OF-GRAPH 
                                  ALL :link OUTGOING)))))

(defn bfs [start visitor]
  (map visitor 
       (seq (.iterator (.traverse start BREADTH END-OF-GRAPH 
                                  ALL :link OUTGOING)))))

;; Tests follow
(use 'clojure.contrib.test-is)

(defn add-n [n]
  (if (zero? n)
    nil
    (do (add-node {:val n})
      (recur (dec n)))))

(deftest get-set-props []
  (with-graph "test-db"
    (set-property (get-root) "foo" 42)
    (is (= 42 (get-property (get-root) "foo")))
    (info (str "Properties: " (get-properties (get-root))))
    (let [n (add-node {"bar" 1234})]
      (info (str "Properties: " (get-properties n)))
      (is (= 1234 (get-property n "bar")))))
  (delete-graph "test-db"))

(deftest simple-query []
  (with-graph "test-db"
    (let [root (get-root)]
      (link-new (get-root ) :foo {"value" 42})
      (let [tgt (path-query root [:foo])]
        (is (= 42 (.getProperty tgt "value"))))))
  (delete-graph "test-db"))

;      (in-tx (link-new (link-new (link-new (link-new root 
;                  :foo) :foo) :foo) :foo {:value 42})
;        (success))
(defn fs-test [] (run-tests (find-ns 'future-store)))
