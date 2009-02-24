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

(ns future-store.utils
  (:import (org.jvnet.inflector Noun)))

(defn- keyword-to-str 
  "Convert a keyword to a string without the ':' prefix."
  [k]
  (.substring (str k) 1))

(defn key-to-sym 
  "Convert a keyword to a symbol without the ':' prefix."
  [k]
  (symbol (keyword-to-str k)))

(defn pluralize [singular & [how-many]]
  (if how-many
    (Noun/pluralOf singular how-many)
    (Noun/pluralOf singular)))

