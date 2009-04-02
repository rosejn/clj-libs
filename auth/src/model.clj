(ns auth.model
  (:require config))

(config/defaults [
            {:login "tom"
             :password "foo"}
            {:login "andy"
             :password "bar"}])

(defmethod authenticate :memory
  [login pass]
  (first (filter #(and (= (% :login) login)
                       (= (% :password) pass)) 
                 USERS)))

(defmethod create-user :memory
  [params] )


