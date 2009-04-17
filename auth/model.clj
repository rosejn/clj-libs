(ns auth.model
  (:use auth))

(defmulti user-create
  (fn [user] *user-model*))

(defmulti user-find
  (fn [user] *user-model*))

(defmulti user-update
  (fn [user] *auth-storage*))

(defmulti user-delete
  (fn [user] *auth-storage*))

(require 'auth.model.memory)
