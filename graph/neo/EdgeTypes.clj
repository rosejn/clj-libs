(ns graph.neo.EdgeTypes
  (:gen-class :main false 
              :extends java.lang.Enum
              :implements [org.neo4j.api.core.RelationshipType]))

(def RELATES (new graph.neo.EdgeTypes "RELATES" 0))

(defmacro defenum
  "Generates and loads a subclass of java.lang.Enum, then
  defs symbols as enumerated instances of that class.

  Example:  (defenum my.package.MyEnum FOO BAR)
            ;; FOO and BAR are now instances of MyEnum

  Java equivalent:  enum MyEnum { FOO, BAR };

  Caveats:
  1. The generated class has no values() method.  
  2. The generated class returns false for Class.isEnum().
  3. Enum.valueOf(Class, String) will not work.
  4. Redefining an enum is allowed, but enumeration resets
     to zero."
  [class & symbols]
  ;; Can't load a class twice, so check first:
  (try (. Class (forName (str class)))  
       (catch java.lang.ClassNotFoundException e
         (gen-class (str class) :extends java.lang.Enum :implements RelationshipType)))
  (cons 'do
        (map (fn [sym val]
                 `(def ~sym (new ~class ~(str sym) ~val)))
             symbols (iterate inc 0))))

;(defenum plasma.graph.PlasmaEdgeType RELATES)
;(gen-class 'graph.EdgeTypes :extends java.lang.Enum :implements RelationshipType)
;(defenum graph.EdgeTypes RELATES)

