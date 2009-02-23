(ns future-store.view
  (:import (org.jvnet.inflector Noun))
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
  (:use future-store jlog))

(def VIEWS (ref {}))

(defn- keyword-to-str 
  "Convert a keyword to a string without the ':' prefix."
  [k]
  (.substring (str k) 1))

(defn key-to-sym 
  "Convert a keyword to a symbol without the ':' prefix."
  [k]
  (symbol (keyword-to-str k)))

(defn pluralize [singular & [how-many]]
  (if how-many
    (Noun/pluralOf singular how-many)
    (Noun/pluralOf singular)))

(defn- add-view-node [label]
  (info "(add-view-node " label ")")
  (check-tx
    (let [root (root-node)
          views (or (path-first root [:fs-views]) 
                    (link-new root :fs-views))]
      (if (path-first views [label])
        (throw (IllegalArgumentException. (str "View already exists: " label))))
      (let [node (link-new views label)]
        (set-property node :id-counter 0)
        node))))

(defn view-root [name]
  (let [plural (keyword (:plural (get @VIEWS name)))
        n      (path-first (root-node) [:fs-views plural])]
    (if (nil? n)
      (add-view-node plural)
      n)))

(defn- view-next-id [base]
  (let [id (get-property base :id-counter)
        next (+ id 1)]
    (set-property base :id-counter next)
    id))

(defn- view-instance [name props]
  (check-tx 
    (let [base (view-root name)
          node (link-new base :instance)
          id   (view-next-id base)]
      (set-property node :id id)
      (doseq [[k v] props]
        (set-property node k v))
      node)))

(defn- view-all
  [name]
  "Get all instances of the given view."
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
(defn create-view [singular & [args]]
  (info "(create-view " singular args ")")
  (let [plural (or (:plural args) (pluralize singular))
        spec {:singular singular
              :plural plural
              :has-one (or (:has-one args) [])
              :has-many (or (:has-one args) [])}]
    (dosync
      (ref-set VIEWS (assoc @VIEWS singular spec)))
    {
     :create (fn [props] (view-instance singular props))
     :all    (fn [] (view-all singular))
     :find   (fn [arg] (view-find singular arg))
     :delete (fn [arg] (view-delete singular arg))
     }))

(defmacro defview [name & [args]]
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
  (let [model-name (nth (re-find #".*\.(.*)" (str full-name)) 1)]
    (info "(view " full-name ") => "  model-name)
    `(.addAlias *ns* (symbol ~model-name) (find-ns ~full-name))))

;; TODO:
;; * relation helpers: create functions that handle the queries necessary 
;;   for one-one and one-many relations in both directions.
;; * properties: define the set of expected properties, with optional types 
;;   and bounds
;; * validation: attach validation functions to views that must pass 
;;   before values are persisted to the DB
;; * view inheritence: the child keeps all the properties and relations 
;;   of it's parent's definition.
(ns future-store.view.test
  (:use clojure.contrib.test-is
     future-store.view
     future-store))

(defview post)
(deftest test-view []
         (test-store 
           (let [base    (view-root "post")
                 counter (get-property base :id-counter)]
             (is (not (nil? base)))
             (is (= 0 counter))
             (let [new-post (post/create {:title "Peppernoten for life"
                                          :text  "Here we go..."})
                   counter (get-property base :id-counter)]
               (is (= 0 (get-property new-post :id)))
               (is (= "Here we go..." (get-property new-post :text)))
               (is (= 1 counter))
               (is (= new-post (post/find 0)))))))

(defview bam)
(deftest test-view-find []
         (test-store
           (dotimes [i 100]
             (bam/create {:a i, :char (char (+ 65 i))}))
           (is (= \A (get-property (bam/find 0) :char)))
           (is (= \C (get-property (bam/find #(= \C (get-property % :char))) :char)))
           (is (= 25 (get-property (bam/find #(= \Z (get-property % :char))) :a)))))

(defview bom)
(deftest test-view-delete []
         (test-store
           (bom/create {:val 1})
           (bom/create {:val 2})
           (is (= 1 (get-property (bom/find 0) :val)))
           (bom/delete 0)
           (is (nil? (bom/find 0)))
           (bom/delete (bom/find 1))
           (is (nil? (bom/find 1)))))

(defn run [] (run-tests (find-ns 'future-store.view.test)))
