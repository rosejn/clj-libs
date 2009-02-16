(ns jlog
  (:import (java.util.logging Logger Level)))

(def CURRENT-LOGGER (Logger/getLogger "default-log"))

(defn info    [& msg] (.info CURRENT-LOGGER (apply str msg)))
(defn warning [& msg] (.warning CURRENT-LOGGER (apply str msg)))
(defn severe  [& msg] (.severe CURRENT-LOGGER (apply str msg)))
