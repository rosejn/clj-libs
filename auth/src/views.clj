(ns login.views
  (:use compojure))

(defn error-for [key]
  (get (flash :errors) key))

(defn error [key msg]
  (flash-assoc :errors (assoc (flash :errors) key msg)))

(defn j-field [type name size]
  [:input {:type type
           :name (str* name)
           :id   (str* name)
           :size size }])
                  
(defn login-panel []
  [:div#login-panel
   [:form#login-form
    [:table
     [:tr
      [:td {:align "right" :size "20"} (label :email "Email:")]
      [:td (j-field "text" :email 15)]]
     [:tr
      [:td {:align "right"} (label :password "Password:")]
      [:td (j-field "password" :password 15)]]]]
   [:span#login-submit 
    (ajax-submit-button "Login" "#login-form" 
                        (action-url :user :login)
                        "#user-panel")]])

(defn user-panel []
  [:div#user-panel
   (if (:user session)
     (user-info)
     (login-panel))])

(defn create []
  (info "Creating a new user: " params)
  (user/create params)
  [:h3 "Thanks for making a Demand Evolution account.  We'll email you shortly to verify the email address, and then your account will be activated."])

(defn create-form []
  [:div#auth-user-create
   (error-messages :auth/user)
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
       (ajax-submit-button "Create Account" "#create-account-form" 
                           (action-url :user :create)
                           "#user-create-form")]]]]])
