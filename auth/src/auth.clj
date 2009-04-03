(ns auth
  ;(:use compojure)
  (:require digest config)
  (:load views))

(config/defaults {
                  :user-model   :memory
                  :auth-storage :session
                  })

(defmulti current-user 
  (fn [] (config :auth-storage)))

(defmethod current-user :session [req]
  (if-let [user (session :user)]
    (find-user (user :id))
    nil))

(defn logged-in? [req]
  (if (current-user req) true false))

(defn with-user [req user]
  (assoc req :user/current-user user))

(defmulti login 
  (fn [req] (user-options req :login-storage)))

(defmethod login :session
  [req]
  (let [params (req :form-params)]
    (if-let [user (authenticate (params :login) (params :password))]
      (with-user req user)
      ))

(defn- prm [req key]
  ((req :params) key))

  (let [login (prm :login)
        email (prm :email)
        password (prm :password)
        confirmation (prm :confirmation)]
        ))

(defmulti authenticate-user
  (fn [login pass] (config :user-model)))

(defmulti create-user
  (fn [params] (config :user-model)))

(defn create [req]
  (create-user (req :params)))

(defn login-url [& [prefix]]
  (str prefix "/login"))

(defn logout-url [& [prefix]]
  (str prefix "/logout"))

(defn create-url [& [prefix]]
  (str prefix "/create"))

(defroutes handlers
           (GET  "*/login" (login-form request))
           (POST "*/login" (login request))
           (GET  "*/logout" (logout request))
           (GET  "*/create" (create-form request))
           (POST "*/create" (create request)))

(defn wrap [app] 
  (fn [req] (app (login req))))

