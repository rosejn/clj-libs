(ns sample
  (:require [auth :as auth])
  (:use compojure))

(defn tpl [content]
  (html 
    (doctype :xhtml-transitional)
    [:html [:head 
            [:title "Login Test App"]]
        [:body content ]]))

(defn home [req]
  (tpl 
    [:div 
     [:p "Home sweet home..."]
     (if-let [user (auth/current-user req)]
       [:p "Hello " (user :login) "!"]
       (auth/login-form))]))

(defn foo [req]
  (if (auth/logged-in? req)
    (tpl (str "You are logged in: " (auth/current-user)))
    (tpl "You are not logged in.")))

(defroutes login-test-app
           (ANY "/" (home request))
           (GET "/foo" (foo request))
           (ANY "/account/*" auth/handlers))

(def authed-app
  (auth/wrap login-test-app))

(run-server {:port 8080}
            "/*" (servlet login-test-app))
;            "/*" (servlet authed-app))
