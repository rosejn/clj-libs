(ns account.util)

(defn request [method uri]
  {:request-method method
   :uri uri})

(defn meta-assoc [obj key val]
  (with-meta obj (assoc (meta obj) key val)))

(defn login-url [& [prefix]]
  (str prefix "/login"))

(defn logout-url [& [prefix]]
  (str prefix "/logout"))

(defn create-url [& [prefix]]
  (str prefix "/create"))

