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

(ns clojure.contrib.graph.internal)

(defn uuid []
  "Creates a random, immutable UUID object that is comparable using the '=' function."
  (. java.util.UUID randomUUID))

"graphs, nodes and edges are structmaps, so add and lookup properties just by using their keys directly on an object."
(defstruct hlist-graph-store :uuid :nodes :edges :names :graph-store)
(defstruct node :uuid :in-edges :out-edges)
(defstruct edge :uuid :src :dest)

(defn hlist-graph [uuid] 
  (struct hlist-graph-store uuid {} {} {} :hlist))

(defn hstore [& args]
  (apply struct (conj args hlist-graph-store)))

(defmethod add-node :hlist [graph node]
  (hstore 
    (:uuid g)
    (assoc (:nodes g) (:uuid node) node)
    (:names g)
    (:edge-count g)
    (:graph-store g)))

(defmethod new-node :hlist [graph props]
  (add-node graph (struct node (uuid))))

