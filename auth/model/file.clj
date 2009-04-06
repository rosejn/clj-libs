(ns auth.model.file)

(defmethod create-user :config
  [params]
  (let [user {:login (params :login)
              :password (params :password)
              :email (params :email)}]
    (config/value :users 
                  (conj (config/value :users) user))))

(defmethod authenticate-user :config
  [login pass]
  (first (filter #(and (= (% :login) login)
                       (= (% :password) pass)) 
                 (config/value :users))))

