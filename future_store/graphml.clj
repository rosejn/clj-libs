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

(ns future-store.graph-xml
  (:use future-store future-store.raw))

(defn gxml-edges [edges]
  (map (fn [e]
    {:tag :edge
     :attrs {:source (get-id (edge-src e))
             :target (get-id (edge-tgt e))}})
       edges))

(defn gxml-node [n]
  (let [tag {:tag :node
             :attrs {:id (get-id n)}}]
    (if (not (zero? (property-count n)))
      (assoc tag 
             :content 
             (map (fn [[k v]] 
                   {:tag :data
                    :attrs {:key k}
                    :content [v]})
                 (get-properties n)))
      tag)))

(defn gxml-nodes [nodes]
  (map gxml-node nodes))

(defn gxml-graph [nodes edges]
  {:tag :graph
   :attrs {:edgedefault "directed"}
   :content (concat (gxml-nodes nodes) (gxml-edges edges))})

(defn- schema-props [type props]
  (map
    (fn [pname]
      {:tag :key
       :attrs {:id pname 
               :for type
               :attr.name pname 
               :attr.type "string"}})
    props))

(defn print-gxml 
  ([] (print-gxml [] []))
  ([nprops] (print-gxml nprops []))
  ([node-props edge-props] 
    (let [nprops (schema-props "node" node-props)
          eprops (schema-props "edge" edge-props)
          schema (concat nprops eprops)]
      (in-tx
        (let [nodes (all-nodes)
              edges (all-edges)
              gxml-tree (gxml-graph nodes edges)
              xml-root {:tag :graphml
                        :attrs {:xmlns "http://graphml.graphdrawing.org/xmlns"}
                        :content (concat schema [gxml-tree])}]
          (success)
          (clojure.xml/emit xml-root))))))
