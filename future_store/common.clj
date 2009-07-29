;;  Copyright (c) Jeff Rose. All rights reserved. The use and
;;  distribution terms for this software are covered by the Common Public
;;  License 1.0 (http://opensource.org/licenses/cpl.php) which can be found
;;  in the file CPL.TXT at the root of this distribution. By using this
;;  software in any fashion, you are agreeing to be bound by the terms of
;;  this license. You must not remove this notice, or any other, from this
;;  software.
;;
;;  rosejn (gmail)
;;  Created 29 July 2009

(ns future-store.common
  (:use (future-store graph)))

(defmethod get-properties :graph [obj]
  (check-tx
    (doall 
      (map (fn [key]
             [key (get-property obj key)])
           (get-property-keys obj)))))

(defmethod property-count :graph [obj]
  (count (get-property-keys obj)))

