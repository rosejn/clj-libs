(ns account.model
  (:use account))

(defmulti user-create
  (fn [user] *user-model*))

(defmulti user-find
  (fn [user] *user-model*))

(defmulti user-update
  (fn [user props] *user-model*))

(defmulti user-delete
  (fn [user] *user-model*))

(require 'account.model.memory)
