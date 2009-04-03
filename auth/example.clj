(ns sample
  (:require auth)
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
     (if-let [user (auth/current-user)]
       [:p "Hello " (user :login) "!"]
       (auth/login-form))]))

(defn foo [req]
  (if (logged-in? req)
    (tpl (str "You are logged in: " (current-user)))

(defroutes login-test-app
           (ANY "/" (home request))
           (GET "/foo" (foo request))
           (ANY "/account/*" (auth/handlers)))

(def authed-app
  (auth/wrap login-test-app))

(run-server {:port 8080}
            "/*" (servlet authed-app))
