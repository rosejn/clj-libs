(ns account.model.memory
  (:use account.model))

;; A simple in memory database of users, mostly just for testing
(def USERS (ref {}))

(defmethod user-create :memory
  [user] 
  (dosync 
    (ref-set USERS (assoc @USERS (user :login) user))))

(defmethod user-find :memory
  [user]
  (if (contains? user :login)
    (@USERS (user :login))
    (first (filter (fn [current]
                     (every? 
                       (fn [[key val]] (= (current key) val)) user))
                   @USERS))))

(defmethod user-update :memory
  [user]
  (let [new-user (merge (@USERS (user :login)) user)]
    (dosync
      (ref-set USERS (assoc @USERS (user :login) new-user)))))

(defmethod user-delete :memory
  [user]
  (dosync
    (ref-set USERS (dissoc @USERS (user :login)))))

