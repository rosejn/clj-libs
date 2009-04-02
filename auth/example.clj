(ns sample
  (:require user)
  (:use compojure))

(defn tpl [content]
  (html 
    (doctype :xhtml-transitional)
    [:html [:head 
            [:title "Login Test App"]]
        [:body content ]]))

(defn home [req]
  (tpl "Home sweet home..."))

(defn foo [req]
  (if (logged-in? req)
    (tpl (str "You are logged in: " (current-user)))

(defroutes login-test-app
           (ANY "/" (home request))
           (GET "/foo" (foo request)))

(def authed-app
  (auth/wrap login-test-app))

(run-server {:port 8080}
            "/*" (servlet authed-app))
