(ns auth.test 
  (:use 
     auth
     clojure.contrib.test-is))

(deftest create-user-test []
  (let [user {:name "Foo Bar"}]
    (is (= "Foo Bar" (user :name)))))

(defn auth-tests [] 
  (run-tests (find-ns 'auth.test)))

;(auth-tests)
