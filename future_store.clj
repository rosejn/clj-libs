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
     (future-store graph neo utils) 
     [future-store.manager :as manager]
     jlog))

(def VIEWS (ref {}))

; NOTE: property keys that start with ':' are turned into keywords...
(defn wrap-entry [k v]
  (let [k (if (.startsWith k ":") (keyword (.substring k 1)) k)]
    (proxy [clojure.lang.IMapEntry] []
      (key [] k) 
      (getKey [] k)
      (val [] v)
      (getValue [] v))))

; Wrap a node so that it implements the Associative interface
; Note, the :node and :edge keys return the base Node or Relationship objects for internal use, but they will not be present when iterating over properties.
(defn to-map [obj]
  (proxy [clojure.lang.Associative clojure.lang.IFn] []
    (count [] (manager/do #(property-count obj)))
    (seq   [] (map (fn [[k v]] (wrap-entry k v)) (manager/do #(get-properties obj))))
    (cons  [[k v]] (manager/do #(set-property obj k v)))
    (empty [] {}) ; Not sure what would make sense here...
    (equiv [o] (and
                 (= (class o) (class obj))
                 (= (.getId o) (.getId obj))))
    (containsKey [k] (manager/do #(has-property? obj k)))
    (entryAt     [k] (wrap-entry k (manager/do #(get-property obj k))))
    (assoc       [k v] (manager/do #(do (set-property obj k v) (to-map obj))))
    (valAt      ([k] (if (or (= :node k) (= :edge k))
                        obj
                        (manager/do #(get-property obj k))))
           ([k d] (manager/do #(if (has-property? obj k) 
                                 (get-property obj k)
                                 d))))
    (invoke ([key] (manager/do #(get-property obj key)))
            ([key default] (manager/do #(if (has-property? obj key) 
                                 (get-property obj key)
                                 default))))))

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
  [name & [create-if-nil?]]
  (let [create-if-nil? (if (or (nil? create-if-nil?) 
                               (false? create-if-nil?)) 
                         false 
                         true)
        plural (keyword (:plural (get @VIEWS name)))
        n      (path-first (root-node) [:fs-views plural])]
    (if (and create-if-nil? (nil? n))
      (create-view-root plural)
      n)))

(defn- view-next-id 
  "Get the next auto-incrementing id for the given view root node."
  [base]
  (let [id (get-property base :id-counter)
        next (+ id 1)]
    (set-property base :id-counter next)
    id))

(defn view-instance 
  "Create a new instance of the named view using props as the property values."  
  [name props]
  (check-tx 
    (let [base (view-root name true)
          node (link-new base :instance)
          id   (view-next-id base)]
      (set-property node :id id)
      (doseq [[k v] props]
        (set-property node k v))
      node)))

(defn view-all
  "Get all instances of the given view."
  [name]
  (let [base (view-root name)]
    (if (nil? base)
      []
      (out-nodes base :instance))))

(defn- to-id [str]
  (if (integer? str)
    str
    (new Integer (re-find #"[0-9]*" str))))

(defn- id-str [str]
  (re-find #"[0-9]*" str))

(defn- id-str? [str]
  (if (string? str)
    (not (empty? (id-str str)))
    false))

(defn- pattern-pred [pattern]
  (fn [node]
    (every? (fn [[prop value]]
              (= value (get-property node prop))) pattern)))

; Returns a predicate function that expects to be passed a node or an edge.  
; args can be either:
;  integer id => (foo/find 42)
;  string  id => (foo/find "42")
;  property pairs => (foo/find :username "Jim" :email "jim@bar.com")
;  filter fn  => (foo/find #(< 365 (get-property % :age)))
(defn- find-predicate [args]
  (let [arg (first args)]
    (cond 
      (integer? arg) #(= arg (to-id (get-property % :id)))
      (id-str? arg)  #(= (to-id arg) (to-id (get-property % :id)))
      (associative? arg) (pattern-pred arg) 
      (keyword? arg) (pattern-pred (apply hash-map args))
      (fn? arg)      arg)))

(defn view-find 
  "Find an instance based on the ID, a pattern map, or by using a predicate that will be passed each instance node until the returns true."
  [name & args]
  (info "(view-find " name " " args ")\nclass: " (class (first args)))
  (let [base (view-root name)]
    (if (nil? base)
      nil
      (let [filter-fn (find-predicate args)]
        (first (filter filter-fn 
                       (out-nodes base :instance)))))))

(defn- object-root [name arg]
  "Find the root node for a specific object given its property-map or its id."
  (cond
    (associative? arg) (:node arg)
    (or (integer? arg) (string? arg)) (view-find name arg)
    true               (throw 
                         (IllegalArgumentException. 
                           (str "Could not find view using: " arg)))))

(defn view-update
  "Update an instance with the given property values."
  [name arg props]
  (let [node (object-root name arg)]
    (info "(view-update: " arg ", " node "\n\n")
    (doseq [[prop value] props]
      (if (not= :id prop)
        (set-property node prop value)))))

(defn view-delete 
  "Delete a record using either the id or the node returned from find."
  [name arg]
  (let [node (object-root name arg)]
    (remove-node node)))

(defn view-spec [vname]
  (get @VIEWS vname))

(defn view-associations [vname]
  (:associations (view-spec vname)))

(defn view-add-association [vname assoc]
  (let [spec       (view-spec vname)
        new-assocs (conj (:associations spec) assoc)]
    ;(info "view-add-association - spec: "
    ;      spec "\n new-assoc:\n"
    ;      new-assocs)
    ;    new-spec   (assoc spec :associations new-assocs)]
    (comment dosync (ref-set VIEWS 
                     (assoc @VIEWS vname new-spec)))))

(defn view-has-one [vname target & [type]]
  (let [type (or type target)]
    (view-add-association vname [:has-one target type])))

(defn view-has-many [vname target & [type]]
  (let [type (or type target)]
    (view-add-association vname [:has-many target type])))

(defn view-add-property [vname pname & [spec]]
  (let [spec (view-spec vname)]))

; TODO: Add index support 
(defn create-view 
  "Creates a new view using the singular name."  
  [singular & [args]]
  ;(info "(create-view " singular " " args ")")
  (let [plural (or (:plural args) (pluralize singular))
        spec {:singular singular
              :plural plural
              :associations []
              }]
    (dosync
      (ref-set VIEWS (assoc @VIEWS singular spec)))
    {
     ; Create and return a new record using the given map
     :create (fn [props] (to-map (manager/do #(view-instance singular props))))


     ; Create a new record using the given map, and return its ID
     :insert (fn [props] (manager/do 
                           #(get-property 
                              (view-instance singular props) 
                              :id)))

     ; Update the given record with the given map values
     ; TODO: Maybe this is redundant given the Associative interface capabilities?
     :update (fn [arg props] (to-map (manager/do 

                                       #(view-update singular arg props))))

     ; Returns a seq of all the records for this view
     :all    (fn [] (map to-map (manager/do #(view-all singular))))

     ; Find a record using either it's ID, a map of properties, or a boolean predicate function
     :find   (fn [& args] 
               (let [result (manager/do #(apply view-find singular args))]
                 (cond
                   (coll? result) (map to-map result)
                   (nil? result) nil
                   (instance? Node result) (to-map result))))

     ; Delete the given record
     :delete (fn [arg] (manager/do #(view-delete singular arg)))

     :has-one (fn [label & [type]]
                (view-has-one singular label type))

     :has-many (fn [label & [type]]
                 (view-has-many label type))
     }))

(defmacro defview 
  "Defines a new view with the given name."  
  [name & [args]]
  (let [name     (str name)
        sym-name (symbol name)
        ns-str   (str (ns-name *ns*) "." name)
        sym-ns   (symbol ns-str)]
    ;(info "(defview " name ") " 
    ;      " => " ns-str)
    `(let [view-funs# (create-view ~name ~args)
           view-ns#   (create-ns (symbol ~ns-str))]
       (doseq [[fun-name# fun#] view-funs#]
         (intern view-ns# (key-to-sym fun-name#) fun#))
       (.addAlias *ns* (symbol ~name) view-ns#))))

(defmacro view [full-name]
  "Import the named view's namespace into the current namespace for handy access."
  (let [str-name (str (eval full-name))
        model-name (nth (re-find #".*\.(.*)" str-name) 1)]
     ;(info "(view " str-name ") => "  model-name "\n")
    `(.addAlias *ns* (symbol ~model-name) (find-ns (symbol ~str-name)))))

(defn view-store 
  "Initializes future store and tells it to open or create the store located at path."  
  [path]
  (manager/start path))

;; TODO:
;; * relation helpers: create functions that handle the queries necessary 
;;   for one-one and one-many relations in both directions.
;; * properties: define the set of expected properties, with optional types 
;;   and bounds
;; * validation: attach validation functions to views that must pass 
;;   before values are persisted to the DB
;; * view inheritence: the child keeps all the properties and relations 
;;   of it's parent's definition.
