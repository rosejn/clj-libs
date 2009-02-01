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
    (try 
      (let [wg-result# (do ~@body)]
        (comment info "with-graph result: " wg-result#)
        wg-result#)
      (finally (close-graph *g*)))))

(defmacro in-tx [& body]
  `(binding [*tx* (.beginTx *g*)]
     (try 
       (let [tx-result# (do ~@body)]
         (comment info "in-tx result: " tx-result#)
         tx-result#)
       (catch Exception e# 
         (do 
           (warning "Exception in transaction: " e#)
           (throw e#)))
     (finally (.finish *tx*)))))

;; Do the enclosing operations inside a transaction if they are not already
;; being performed in an outer transaction created by the user.
(defmacro check-tx [& body]
  `(if *tx* 
     (do ~@body)
     (do (in-tx *g* 
       (let [ctx-result# (do ~@body)]
         (comment info "check-tx result: " ctx-result#)
         (success)
         ctx-result#)))))

(defn root-node [] 
  (check-tx (.getReferenceNode *g*)))

(defn find-node [id]
  (info "(find-node " id ")")
  (check-tx 
    (try 
      (.getNodeById *g* id)
      (catch NotFoundException e nil))))

(defn find-edge [id]
  (check-tx 
    (try 
      (.getRelationshipById *g* id)
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
  (.getId obj))

(defn get-property [obj key]
  (info "(get-property " (get-id obj) " " key ")")
  (check-tx (.getProperty obj (str key))))

(defn get-properties [obj]
  (info "(get-properties " (get-id obj) ")")
  (check-tx 
    (seq (.getPropertyKeys obj))))

(defn set-property [obj key value]
  (info "(set-property " (get-id obj) " {" key value "})")
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
      (doall (map (fn [[k v]] (set-property n k v)) props))
    n)))

(defn remove-node [#^Node n]
  (info "(remove-node " (get-id n) ")")
  (try 
    (check-tx 
      (.delete n))
      true
    (catch NotFoundException e false)))

(defn edge-type [#^Keyword n]
  (proxy [RelationshipType] []
    (name [] (name n))))

(defn add-edge [#^Node src #^Keyword label #^Node dest & [props]]
  (info "(add-edge " (get-id src) " " label " " (get-id dest) " " props ")")
  (check-tx 
    (let [edge (.createRelationshipTo src dest (edge-type label))]
    (doall (map (fn [[k v]] (set-property edge k v)) props))
      edge)))

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
  (println "(edge-filter " label ")")
  (fn [edge] (= label (keyword (edge-label edge)))))

(defn- get-edges [#^Node n #^Direction direction label]
  (info "(get-edges " (get-id n) " " direction " " label ")")
  (check-tx 
    (let [edges (.getRelationships n direction)
          filtered (if label
                     (doall (filter (edge-filter label) (seq edges)))
                     (seq edges))]
      (info "get-edges edges: " (count (seq edges)))
      (info "get-edges filtered: " (count (seq filtered)))
      filtered)))

(defn in-edges [#^Node n & [label]]
  (get-edges n INCOMING label))

(defn out-edges [#^Node n & [label]]
  (info "(out-edges " label ")")
  (let [edges (get-edges n OUTGOING label)]
    (info "out-edges result: " (count (seq edges)))
    edges))

(defn in-nodes [#^Node n & [label]]
  (map (fn [edge] (.getStartNode edge))
       (in-edges n label)))

(defn out-nodes [#^Node n & [label]]
  (info "(out-nodes " label ")")
  (map (fn [edge] (.getEndNode edge))
       (out-edges n label)))

(defn path-query [#^Node start path]
  (info "(path-query " (get-id start) " " path ")")
  (check-tx
    (if (empty? path)
      (do
        (info "finished query at node: " start)
        start)
      (let [label (first path)
            children (out-nodes start label)
            blah (info "path-query label: " label " children: " (count children))
            result (doall (map (fn [child] (path-query child (rest path))) children))]
        (info "path-query result: " result)
        (flatten result)))))

(defn dfs [#^Node start visitor]
  (map visitor 
       (seq (.iterator (.traverse start DEPTH END-OF-GRAPH 
                                  ALL :link OUTGOING)))))

(defn bfs [#^Node start visitor]
  (map visitor 
       (seq (.iterator (.traverse start BREADTH END-OF-GRAPH 
                                  ALL :link OUTGOING)))))

;; Tests follow
(use 'clojure.contrib.test-is)

(deftest get-set-props []
  (with-graph "test-db"
    (set-property (root-node) "foo" 42)
    (is (= 42 (get-property (root-node) "foo")))
    (info (str "Properties: " (get-properties (root-node))))
    (let [n (add-node {"bar" 1234})]
      (info (str "Properties: " (get-properties n)))
      (is (= 1234 (get-property n "bar")))))
  (delete-graph "test-db"))

(deftest add-remove []
  (with-graph "test-db"
    (let [n (add-node)
          id (get-id n)]
      (is (= id (get-id (find-node id))))
      (remove-node n)
      (is (nil? (find-node id)))))
  (delete-graph "test-db"))

(defn n-children [parent n label]
  (info "(n-children " (get-id parent) " " n " " label ")")
  (if (zero? n)
    parent
    (do
      (link-new parent label)
      (recur parent (- n 1) label))))

(deftest simple-query []
  (with-graph "test-db"
    (let [root (root-node)]
      (in-tx 
        (n-children 
          (link-new (link-new (link-new (link-new root 
          :foo) :foo) :foo) :foo {"value" 42})
          5
          :bar)
        (success))
      (let [tgt (first (path-query root [:foo :foo :foo :foo]))
            val (.getProperty tgt "value")
            edge-count (count (out-edges tgt :bar))
            spread-count (count (path-query root [:foo :foo :foo :foo :bar]))]
        (info "simple-query tgt: " tgt)
        (is (= 42 val))
        (is (= 5 edge-count))
        (is (= 5 spread-count)))))
  (delete-graph "test-db"))

;        (success))
(defn fs-test [] (run-tests (find-ns 'future-store)))
