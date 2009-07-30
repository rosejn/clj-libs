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

(ns future-store.mem
  (:use (future-store graph)))

(defn- uuid 
  "Creates a random, immutable UUID object that is comparable using the '=' function."
  [] (. java.util.UUID randomUUID))

(defstruct mem-graph-store :uuid :nodes :edges :root :tag)
(defstruct mem-node :uuid :in-edges :out-edges :props :tag)
(defstruct mem-edge :uuid :src :dest :label :props :tag)

(defn mem-store [] 
  (struct mem-graph-store (uuid) {} {} nil :future-store.graph/mem))

; TODO: finish this and add constructors for other structs
(defn node [&args]
  (apply struct mem-node args))

(defmacro with-store [store & body ]
  `(binding [*store* (or store (mem-store))]
     (do ~@body)))

(defmethod root-node :mem [] 
  (:root *store*))

(defmethod find-node :mem [id]
  (get (:nodes *store*) id))

(defmethod find-edge :mem [id]
  (get (:edges *store*) id))

(defmethod edge-label :mem [e]
  (:label e))

(defmethod edge-src :mem [e]
  (:src e))

(defmethod edge-tgt :mem [e]
  (:tgt e))
  
(defmethod get-id :mem [obj]
  (:uuid obj))

(defmethod has-property? :mem [obj key]
  (contains? (:props obj) key))

(defmethod get-property :mem [obj key]
  (get (:props obj) key))

(defmethod get-property-keys :mem [obj]
  (keys (:props obj)))

(defmethod set-property :mem [obj key value]
  (dosync
    (alter assoc *store* 

(defmethod node-count :mem []
  (count (:nodes *store*)))

(defmethod edge-count :mem []
  (count (:edges *store*)))

(defmethod nodes :mem []
  (map (fn [[k v]] k) (:nodes *store*)))

(defmethod edges :mem []
  (map (fn [[k v]] k) (:edges *store*)))

(defmethod in-nodes :mem [n]
  (map (fn [[k v]] 

(defmethod add-node :mem [node-props]
  (let [props (or (first node-props) {})
        nid (uuid)
        node (struct mem-node nid)]
    (merge *store* 
           {:nodes (assoc (:nodes *store*) nid (merge node props))})))

(defmethod remove-node :mem [node-id]
  (merge *store* 
         {:nodes (dissoc (:nodes *store*) node-id)}))

(defmethod add-edge :mem [edge-props src-id dst-id]
  (let [props (or (first edge-props) {})
        id (uuid)
        edge (struct mem-edge id src-id dst-id)]
    (merge *store* 
           {:edges (assoc (:edges *store*) id (merge edge props))})))

(defmethod remove-edge :mem [edge-id]
  (merge *store* 
         {:edges (dissoc (:edges *store*) edge-id)}))

