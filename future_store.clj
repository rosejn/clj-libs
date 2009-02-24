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
  (:use 
     (future-store raw manager utils) 
     jlog))

(def VIEWS (ref {}))

(defn- create-view-root [label]
  (check-tx
    (let [root (root-node)
          views (or (path-first root [:fs-views]) 
                    (link-new root :fs-views))]
      (if (path-first views [label])
        (throw (IllegalArgumentException. (str "View already exists: " label))))
      (let [node (link-new views label)]
        (set-property node :id-counter 0)
        node))))

(defn view-root 
  "Retrieve the root node for the named view.  The node will be created if it does not already exist."
  [name]
  (let [plural (keyword (:plural (get @VIEWS name)))
        n      (path-first (root-node) [:fs-views plural])]
    (if (nil? n)
      (create-view-root plural)
      n)))

(defn- view-next-id 
  "Get the next auto-incrementing id for the given view root node."
  [base]
  (let [id (get-property base :id-counter)
        next (+ id 1)]
    (set-property base :id-counter next)
    id))

(defn- view-instance 
  "Create a new instance of the named view using props as the property values."  
  [name props]
  (check-tx 
    (let [base (view-root name)
          node (link-new base :instance)
          id   (view-next-id base)]
      (set-property node :id id)
      (doseq [[k v] props]
        (set-property node k v))
      node)))

(defn- view-all
  "Get all instances of the given view."
  [name]
  (let [base (view-root name)]
    (out-nodes base :instance)))

(defn- view-find 
  "Find an instance based on the ID or by using a predicate that will be passed each instance node until the returns true."
  [name arg]
  (let [base (view-root name)
        filter-fn (cond 
                    (fn? arg) arg
                    (integer? arg) #(= arg (get-property % :id)))]
    (first (filter filter-fn 
                   (out-nodes base :instance)))))

(defn- view-delete 
  "Delete a record using either the id or the node returned from find."
  [name arg]
  (let [node (cond
               (integer? arg) (view-find name arg)
               (instance? Node arg) arg)]
    (remove-node node)))

; TODO: Add index support 
(defn create-view 
"Creates a new view using the singular name."  
  [singular & [args]]
  (info "(create-view " singular args ")")
  (let [plural (or (:plural args) (pluralize singular))
        spec {:singular singular
              :plural plural
              :has-one (or (:has-one args) [])
              :has-many (or (:has-one args) [])}]
    (dosync
      (ref-set VIEWS (assoc @VIEWS singular spec)))
    {
     :create (fn [props] (manager-do #(view-instance singular props)))
     :all    (fn [] (manager-do #(view-all singular)))
     :find   (fn [arg] (manager-do #(view-find singular arg)))
     :delete (fn [arg] (manager-do #(view-delete singular arg)))
     }))

(defmacro defview 
"Defines a new view with the given name."  
  [name & [args]]
  (let [name (str name)
        sym-name (symbol name)
        ns-str (str (ns-name *ns*) "." name)
        sym-ns (symbol ns-str)]
    (info "(defview " name ") " 
          " => " ns-str)
    `(let [view-funs# (create-view ~name ~args)
           view-ns#   (create-ns (symbol ~ns-str))]
       (doseq [[fun-name# fun#] view-funs#]
         (intern view-ns# (key-to-sym fun-name#) fun#))
       (.addAlias *ns* (symbol ~name) view-ns#))))

(defmacro view [full-name]
"Import the named view's namespace into the current namespace for handy access."
  (let [model-name (nth (re-find #".*\.(.*)" (str full-name)) 1)]
    (info "(view " full-name ") => "  model-name)
    `(.addAlias *ns* (symbol ~model-name) (find-ns ~full-name))))

(defn view-store 
"Initializes future store and tells it to open or create the store located at path."  
  [path]
  (manager-start path))

;; TODO:
;; * relation helpers: create functions that handle the queries necessary 
;;   for one-one and one-many relations in both directions.
;; * properties: define the set of expected properties, with optional types 
;;   and bounds
;; * validation: attach validation functions to views that must pass 
;;   before values are persisted to the DB
;; * view inheritence: the child keeps all the properties and relations 
;;   of it's parent's definition.
