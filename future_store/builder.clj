;;  Copyright (c) Jeff Rose. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  rosejn (gmail)
;;  Created 23 Feb 2009

(ns future-store.builder
  (:use future-store.raw))

(defn n-children [parent n label]
  (if (zero? n)
    parent
    (do
      (link-new parent label)
      (recur parent (- n 1) label))))

(defn create_path [root edge-list]
  (if (empty? edge-list)
    root
    (recur (link-new root (first edge-list)) (rest edge-list))))

(defn build-tree [parent depth spread label]
  (if (> depth 1)
    (dotimes [i spread]
      (build-tree (link-new parent label) (- depth 1) spread label))))

(defn load-tree
  "Load a tree representation of a sub-graph using maps and vectors.  Map properties with simple values will be stored as regular properties, but map values and vectors of maps will be stored as edges to new nodes. For example:
          {:app {
                 :foo {:app-name \"foo\"}
                 :bar {:app-name \"bar\"}
                }
           :net {
                  :peer [{:peer-id 1} {:peer-id 2}]
                }
          }"
  [parent label root] 
  (let [node (link-new parent label)]
    (doseq [[key val] root]
      (cond
        (map? val) (load-tree node key val)
        (and (vector? val) 
             (every? map? val)) (doseq [child val]
                                  (load-tree node key child))
        :else (set-property node key val)))))
