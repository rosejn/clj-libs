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

(ns future-store.dot
  (:use future-store future-store.raw))

(defn- dot-nodes [nodes output & [options]]
  (if (empty? nodes)
    output
    (let [node (first nodes)
               name (if (has-property? node) 
                      (get-property node :name) 
                      (get-id node))]
      (recur (rest nodes) (str output "\n\t\"" name "\"") options))))

(defn- dot-edges [edges output & [options]]
  (if (empty? edges)
    output
    (let [edge (first edges)
               name (if (has-property? edge) 
                      (get-property edge :name) 
                      (get-id edge))
               src  (edge-src edge)
               tgt  (edge-tgt edge)]
      (recur (rest edges) (str output "\n\t\"" name "\"") options))))

(defn print-dot [nodes edges & [options]]
  (in-tx
    (let [name (or (:name options) "\"future-store\"")
               output (str "digraph " name " {\n" 
                           (dot-nodes nodes nil options) "\n"
                           (dot-edges edges nil options) "\n}\n")]
      (success)
      output)))

