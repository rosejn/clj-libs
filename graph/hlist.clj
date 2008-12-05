;;  Copyright (c) Jeff Rose. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  graph.clj
;;
;;  A Clojure graph library
;;
;;  See clojure.contrib.graph.test for an example
;;
;;  rosejn (gmail)
;;  Created 15 November 2008

(defstruct hlist-graph-store :uuid :nodes :edges :names :graph-store)
(defstruct hlist-node :uuid :in-edges :out-edges)
(defstruct hlist-edge :uuid :src :dest)

(defn hlist-graph [] 
  (struct hlist-graph-store (uuid) {} {} {} :hlist))

(defmethod get-root :hlist [g] 
  (:root (:names g)))

(defmethod get-node :hlist [g node-id]
  (get (:nodes g) node-id))

(defmethod get-edge :hlist [g edge-id]
  (get (:edges g) edge-id))

(defmethod node-count :hlist [g]
  (count (:nodes g)))

(defmethod edge-count :hlist [g]
  (count (:edges g)))

(defmethod nodes :hlist [g]
  (map (fn [[k v]] k) (:nodes g)))

(defmethod edges :hlist [g]
  (map (fn [[k v]] k) (:edges g)))

(defmethod add-node :hlist [g node-props]
  (let [props (or (first node-props) {})
        nid (uuid)
        node (struct hlist-node nid)]
    (merge g 
           {:nodes (assoc (:nodes g) nid (merge node props))})))

(defmethod remove-node :hlist [g node-id]
  (merge g 
         {:nodes (dissoc (:nodes g) node-id)}))

(defmethod add-edge :hlist [g edge-props src-id dst-id]
  (let [props (or (first edge-props) {})
        id (uuid)
        edge (struct hlist-edge id src-id dst-id)]
    (merge g 
           {:edges (assoc (:edges g) id (merge edge props))})))

(defmethod remove-edge :hlist [g edge-id]
  (merge g 
         {:edges (dissoc (:edges g) edge-id)}))

