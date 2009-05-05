(ns sample
  (:require [account :as account])
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
     (if-let [user (current-user req)]
       [:p "Hello " (user :login) "!"]
       (login-form))]))

(defn foo [req]
  (if (logged-in? req)
    (tpl (str "You are logged in: " (current-user)))
    (tpl "You are not logged in.")))

(defroutes login-test-app
           (ANY "/" (home request))
           (GET "/foo" (foo request))
           (ANY "/account/*" account/handlers))

(def account-app
  (account/wrap login-test-app))

(run-server {:port 8080}
            "/*" (servlet login-test-app))
;            "/*" (servlet accounted-app))
