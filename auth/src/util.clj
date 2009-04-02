(in-ns 'auth)

(defn meta-assoc [obj key val]
  (with-meta obj (assoc (meta obj) key val)))

(defn with-user-options [req options]
  (meta-assoc req :user/options options))

(defn user-options [req key]
  (if-let [val (get (get (meta req) :user/options) key)]
    val
    (get DEFAULT-OPTIONS key))

