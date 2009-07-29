(ns plasma.core
  (:use [clojure.contrib.javalog :only (log)])
  (:use net))

(def DEFAULT-PORT 5678)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Query Operators
;
; Questions:
; * What are the node arguments?
;  - reference to in-db node object
;  - abstract object containing a node-id for the current db
; * What's returned by leaves and sort?
;  - constructed result graph
;  - sequence
; * What do sort and other set operations accept as arguments?
;  - sequence
;  - previous result graph with identical edge to all nodes
; * What does avg return?
;  - single value
;  - node containing a value

; e.g.: (sort (leaves result-root :any) "score")

(defn traverse 
  "Return the set of nodes targeted by all edges matching the edge-predicate"
  {:tag clojure.lang.PersistentHashSet}
  [node edge-p])

(defn path
  "Return the traversed tree defined by the ordered edge-predicate list."
  [node edges])

(defn leaves
  "Return the set of leaf nodes targeted by the outer-most edge-predicate."
  [node edges])

(defn sort
  "Sort a set of nodes based on the specified property-name or sorting function."
  [prop])

(defn min
  "Return the node containing the minimum value for the given property."
  [prop])

(defn max
  "Return the node containing the maximum value for the given property."
  [prop])

(defn avg
  "Return the a node containing the average value for the given property."
  [prop])

(defn union
  "Return the union of two sets of nodes."
  [a b])

(defn intersection
  "Return the intersection of two sets of nodes."
  [a b])

(defn difference
  "Return the difference of two sets of nodes."
  [a b])

(defn iterate
  "Perform a sub-query N times, locally."
  [n query])

(defn recurse
  "Perform a sub-query N times, where each successive iteration occurs on the node which last completed the sub-query."
  [n query])

(defn construct
  "Construct a result graph by binding intermediate nodes in a path expression and then forming new edges in the result."
  [node bind-path contruct-path])

(defn insert
  "Connect a new node to an existing node with the specified edge."
  [node edge new-node])

(defn insert-leaf
  "Connect a new node at the end of a path traversal, optionally creating intermediate nodes."
  [node edge-seq new-node])

(defn delete
  "Delete a node and all of its edges."
  [node])

(defn delete-edge
  "Delete an edge."
  [edge])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Query Graph Construction
; 

(defn- query-node [spec]
  (let [operator (first spec)
        arguments (rest spec)]
    (

(defn build-query [spec]
  )

(defmacro query [& query-spec]
  (build-query query-spec))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Query Manager
; 
; Questions:
; * How is a result appended to the correct location in a result graph?
; - All sub-queries are spawned as a completely separate query, so when the remote result comes back it will be the entire result.  Then, the operators in the parent query will have access to these results to use them for further processing.

(defn do-query
  "Start processing a query. Returns a future representing the eventual result of this query."
  [query & [config]])

(defn- query-result
  "Add a result to the query envelope."
  [query-id result])

(defn- await-queries
  "Put query into a pending state, waiting for all sub-queries to complete or timeout."
  [parent-query-id sub-query-ids])

; Query Envelope
; * original query graph
; * working query (optimized, etc...)
; * result graph
; * status (pending, processing, complete)
; * source 
;  - local: set future value
;  - parent-query-id: notify parent 
;  - remote-peer: transmit result
; * configuration
;  - credentials
;  - timeouts
;  - error handlers
;  - networking info
; * query log
; * result cursor?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Query Processor

(defn process-query
  "Traverse the query graph, executing the operators according to their dependencies, and eventually setting a query result."
  [query])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Query Optimizer

(defn optimize-query
  "Rewrite the query graph to improve performance while maintaining identical semantics."
  [query-id])

(defn net-optimize-query
  "Rewrite the query to improve network oriented performance while maintaining semantics."
  [query-id])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Network Transport

;(defn transport-init
;  (add-log-file "global" "client.log")
;  (log :info "Starting client...") 
;  (plasma.net/start-client "localhost" DEFAULT-PORT))

(defn send-query
  "Send the query to a remote peer for processing."
  [query-id peer-info])

(defn send-result
  "Send a query result to a remote peer."
  [query-id result peer-spec])

(defn listen
  "Accept remote queries."
  [& [transport-spec]]
  (add-log-file "global" "listener.log")
  (log :info "Starting listener...") 
  (net/listener DEFAULT-PORT (partial graph-listener db)))

