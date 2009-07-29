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

(defstruct mem-graph-store :uuid :nodes :edges :root :store-type)
(defstruct mem-node :uuid :in-edges :out-edges :props)
(defstruct mem-edge :uuid :src :dest :props)

(defn mem-store [] 
  (struct mem-graph-store (uuid) {} {} nil :mem))

(defmacro with-store [store & body ]
  `(binding [*store* (or store (mem-store))]
     (do ~@body)))

;(defmethod graph-id :mem [g]
;  (:uuid g))

(defmethod node-id :mem [n]
  (:uuid n))

(defmethod edge-id :mem [e]
  (:uuid e))

(defmethod get-root :mem [g] 
  (:root (:names g)))

(defmethod get-node :mem [g id]
  (get (:nodes g) id))

(defmethod get-edge :mem [g id]
  (get (:edges g) id))

(defmethod node-count :mem [g]
  (count (:nodes g)))

(defmethod edge-count :mem [g]
  (count (:edges g)))

(defmethod nodes :mem [g]
  (map (fn [[k v]] k) (:nodes g)))

(defmethod edges :mem [g]
  (map (fn [[k v]] k) (:edges g)))

(defmethod in-nodes :mem [g n]
  (map (fn [[k v]] 

(defmethod add-node :mem [g node-props]
  (let [props (or (first node-props) {})
        nid (uuid)
        node (struct mem-node nid)]
    (merge g 
           {:nodes (assoc (:nodes g) nid (merge node props))})))

(defmethod remove-node :mem [g node-id]
  (merge g 
         {:nodes (dissoc (:nodes g) node-id)}))

(defmethod add-edge :mem [g edge-props src-id dst-id]
  (let [props (or (first edge-props) {})
        id (uuid)
        edge (struct mem-edge id src-id dst-id)]
    (merge g 
           {:edges (assoc (:edges g) id (merge edge props))})))

(defmethod remove-edge :mem [g edge-id]
  (merge g 
         {:edges (dissoc (:edges g) edge-id)}))

