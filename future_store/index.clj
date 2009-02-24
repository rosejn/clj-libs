(ns future-store.index
  (:import (org.neo4j.util.index SingleValueIndex))
  (:use future-store))

;; TODO: The index component is not thread safe yet, so this might need to be
;; guarded by an agent or something...

(defn index-property [node key & [label]]
  (check-tx 
    (let [idx-name (str (get-id node) "-[" label "]-" key)
          idx-node (link-new node :index {:index-name idx-name,
                                          :index-edge-label (str label),
                                          :index-prop-key (str key)})
          index (new SingleValueIndex idx-name idx-node *store*)
          children (out-nodes node label)]
      (doseq [child children]
        (.index index child (get-property child key)))
      index)))

(defn lookup [index value]
  (.getSingleNodeFor index value))

