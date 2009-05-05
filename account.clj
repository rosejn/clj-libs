(ns account
  (:use (account util)
     (compojure ns-utils)
     (compojure.http routes session helpers)
     web-helpers)
  (:require (account [digest :as digest]
                  [view :as view]))
  (:import (java.util Date)))

(def *user-model*      :memory)
(def *account-storage* :session)

(use 'account.model)

;; Based on the current session
(defn- session-login [req]
  ((req :session) :user))

;; Based on Login and Password
(defn authenticated? [user text-password]
  (let [encrypted-pass (digest/encrypt-password text-password (user :salt))]
    (= (user :password) encrypted-pass)))

;; Based on a token stored in a cookie to remember the user 
(defn- expired? [expiration]
  (< (.getTime (Date.)) expiration))

(defn- cookie-login [req]
  (if-let [token (get (req :cookies) :account-token)]
    (let [user (user-find {:account-token token})]
      (if (and
            (not (nil? user))
            (expired? (user :account-token-expiration)))
        user))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Interface Follows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-user-model! [model-key]
  (def *user-model* model-key))

(defn login [user-params]
  (if-let [user (user-find (dissoc user-params :password))]
    (if (authenticated? user (user-params :password))
      user)))

(defn session-remember [user]
  (session-assoc :user user))

(defn cookie-remember [user expires]
  (let [token (digest/make-token)]
    (user-update user {:account-token token
                       :account-token-expiration expires})
    (cookie {:name  "account-token"
             :value token
             :expires expires })))

(defn remember [user expires]
  [(session-remember user)
   (cookie-remember user expires)])

(defn logout []
  (session-dissoc :user))

; Assign each user a salt on creation
(defn create [user]
  (let [salt     (digest/make-token)
        password (digest/encrypt-password (user :password) salt)
        user (merge user {:salt salt :password password})]
  (user-create user)))

(defn current-user [req]
  (req :user))

(defn logged-in? [req]
  (if (current-user req) true false))

(defn assoc-user [req]
  (assoc req :user 
         (or (session-login req)
             (cookie-login  req))))

(defroutes handlers
           (GET  "*/login" (view/login-form request))
           (POST "*/login" (login request))
           (GET  "*/logout" (logout request))
           (GET  "*/create" (view/create-account-form request))
           (POST "*/create" (create request)))

(defn with-user [app] 
  (fn [req] (app (assoc-user req))))

