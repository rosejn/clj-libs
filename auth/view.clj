(ns auth.view
  (:use 
     auth (auth util)
     web-helpers
     compojure.http.session
     compojure.str-utils
     compojure.html))

(defn error-for [req key]
  ((req :flash) :errors) key)

(defn error [req key msg]
  (let [flash (req :flash)]
    (flash-assoc :errors (assoc flash :errors) key msg)))

(defn sized-text-field [type name size]
  [:input {:type type
           :name (str* name)
           :id   (str* name)
           :size size }])
                  
(defn login-form []
  [:div#login-panel
   [:form#login-form {:method "POST" :action (login-url)}
    [:table
     [:tr
      [:td {:align "right" :size "20"} (label :login "login:")]
      [:td (sized-text-field "text" :login 15)]]
     [:tr
      [:td {:align "right"} (label :password "Password:")]
      [:td (sized-text-field "password" :password 15)]]]]
   [:span#login-submit (submit-button "Login")]])

(defn create-account-form []
  [:div#auth-user-create
   (error-for :auth/user)
   [:form {:id "auth-user-create-form"}
    [:table
     [:tr
      [:td {:align "right"}
       (label :login "Login:") " "]
      [:td (text-field :login)]]

     [:tr
      [:td {:align "right"}
       (label :email "Email:") " "]
      [:td (text-field :email)]]

     [:tr
      [:td {:align "right"}
       (label :password "Password:") " "]
      [:td (password-field :password)]]

     [:tr
      [:td {:align "right"}
       (label :confirmation "Password confirmation:") " "]
      [:td (password-field :confirmation)]]

     [:tr
      [:td {:align "right" :colspan 2}
       (submit-button "Create Account")]]]]])
