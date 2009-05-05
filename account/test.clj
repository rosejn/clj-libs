(ns account.test 
  (:use 
     account
     clojure.contrib.test-is))

(deftest create-user-test []
  (let [user {:name "Foo Bar"}]
    (is (= "Foo Bar" (user :name)))))

(defn account-tests [] 
  (run-tests (find-ns 'account.test)))

;(account-tests)
