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

(ns clojure.contrib.graph
  (:use clojure.contrib.graph.internal))

(def default-graph-store hlist-graph)

(defn graph 
  ([] (default-graph-store))
  ([graph-store] (graph-store)))

(defmulti add-node :graph-store)

(println "graph.clj reloaded...")

