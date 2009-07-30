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
  (:import (java.io File))
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

(declare delete-dir)

(defn- delete-files [file-list]
  (if (not (empty? file-list))
    (let [f (first file-list)]
      (if (.isDirectory f) (delete-dir f) (.delete f))
      (recur (rest file-list)))))

(defn- delete-dir [dir]
  (if (.exists dir)
    (do 
      (let [files (.listFiles dir)]
        (delete-files files))
      (.delete dir))))

(defn delete-store [path]
  (let [dir (File. path)]
    (delete-dir dir)))

(defmacro test-store 
"Executes body within the context of a store named \"test-store\" that will be automatically deleted when body completes or an exception occurs."  
  [& body]
  `(try 
     (future-store.neo/with-store "test-store"
       ~@body)
     (finally (delete-store "test-store"))))

(defmacro test-manager
"Executes body within the context of a store named \"test-store\" that will be automatically deleted when body completes or an exception occurs."  
  [& body]
  `(try 
     (future-store/view-store "test-store")
       (do 
         ~@body)
     (finally (do
                (future-store.manager/stop)
                (delete-store "test-store")))))

; Thankfully found in the compojure project
(defn immigrate
 "Create a public var in this namespace for each public var in the
 namespaces named by ns-names. The created vars have the same name, value,
 and metadata as the original except that their :ns metadata value is this
 namespace."
 [& ns-names]
 (doseq [ns ns-names]
   (require ns)
   (doseq [[sym var] (ns-publics ns)]
     (let [sym (with-meta sym (assoc (meta var) :ns *ns*))]
       (if (.isBound var)
         (intern *ns* sym (var-get var))
         (intern *ns* sym))))))
