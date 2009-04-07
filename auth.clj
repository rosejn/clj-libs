(ns auth
  (:use (auth util authentication)
     (compojure ns-utils)
     (compojure.http routes session))
  (:require (auth [digest :as digest]
                  [view :as view]))
  (:import (java.util Date)))

(def *user-model*   :memory)
(def *auth-storage* :session)

(use 'auth.model)

;; Based on the current session
(defn- session-login [req]
  ((req :session) :user))

;; Based on Login and Password
(defn- authenticated? [user password]
  (let [encrypted-pass (encrypt-password password (user :salt))]
    (= (user :password) encrypted-pass)))

;; Based on a token stored in a cookie to remember the user 
(defn- expired? [expiration]
  (< (.getTime (Date.)) expiration))

(defn- cookie-login [req]
  (if-let [token ((request :cookies) :auth-token)]
    (let [user (user-find {:token token})]
      (if (and
            (not (nil? user))
            (expired? (user :token-expiration)))
        user))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Interface Follows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn login [req]
  (let [params (req :params)
        {:keys [login passsword]} params
        user   (user-find {:login login})]
    (if (authenticated? user password)
      (session-assoc :user user))))

(defn logout []
  (session-dissoc :user))

;TODO: Write some helpers to make it easier to specify how long a user should be remembered...
(defn remember-for [req time]
  (let [user  (current-user req)
        token (digest/make-token)]
    (user-save-token (user :login) token)
    (set-cookie :auth-user token)))

(defn create [req]
  (user-create (req :params)))

(defn logged-in? [req]
  (if (req :user) true false))

(defn with-user [req]
  (assoc req :user 
         (or (session-login req)
             (cookie-login  req))))

(defroutes handlers
           (GET  "*/login" (view/login-form request))
           (POST "*/login" (login request))
           (GET  "*/logout" (logout request))
           (GET  "*/create" (view/create-account-form request))
           (POST "*/create" (create request)))

(defn wrap [app] 
  (fn [req] (app (with-user req))))
