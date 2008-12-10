;;  Copyright (c) Jeff Rose. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  neo.clj
;;
;;  A neo4j back-end to the graph library.
;;
;;  rosejn (gmail)
;;  Created 15 November 2008

(ns graph.neo
  (:use neo4j))

(with-neo "test"
  (tx
    (let [c (new-node) b (new-node)]
      (relate (top-node) :customers c)
      (relate c :customer b)
      (.setProperty b "name" "Bob")
      (success))))
