(ns proto)

; Prototype based objects for Clojure

(defn base [obj]
  (let [slots {:kind "Base"}]
  (proxy [clojure.lang.Associative] []
    (count [] (manager-do #(property-count obj)))
    (seq   [] (map (fn [[k v]] (wrap-entry k v)) (manager-do #(get-properties obj))))
    (cons  [[k v]] (manager-do #(set-property obj k v)))
    (empty [] {}) ; Not sure what would make sense here...
    (equiv [o] (and
                 (= (class o) (class obj))
                 (= (.getId o) (.getId obj))))
    (containsKey [k] (manager-do #(has-property? obj k)))
    (entryAt     [k] (wrap-entry k (manager-do #(get-property obj k))))
    (assoc       [k v] (manager-do #(do (set-property obj k v) (wrap-assoc obj))))
    (valAt       ([k] (if (or (= :node k) (= :edge k))
                        obj
                        (manager-do #(get-property obj k))))
           ([k d] (manager-do #(if (has-property? obj k) 
                                 (get-property obj k))
                              d)))))
