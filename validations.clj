(ns validations)

(def validations
  {:inclusion  "is not included in the list"
   :exclusion  "is reserved"
   :invalid    "is invalid"
   :confirmation  "doesn't match confirmation"
   :accepted      "must be accepted"
   :empty      "can't be empty"
   :blank      "can't be blank"
   :too_long   "is too long (maximum is %d characters)"
   :too_short  "is too short (minimum is %d characters)"
   :wrong_length  "is the wrong length (should be %d characters)"
   :taken         "has already been taken"
   :not_a_number  "is not a number"
   :greater_than  "must be greater than %d"
   :greater_than_or_equal_to  "must be greater than or equal to %d"
   :equal_to   "must be equal to %d"
   :less_than  "must be less than %d"
   :less_than_or_equal_to  "must be less than or equal to %d"
   :odd   "must be odd"
   :even  "must be even" 
   })

(def valid? [value spec]
  (every? (fn [[validation args]] (apply validation value args))
          spec))

;; Numeric validations
(defn between? [val low high]
  (and (> val low)
       (< val high)))

;; String validations
(defn valid-length [val & args]

(defn non-empty? [val]
  (not (empty? val)))

(defn matches? [val pattern]
  (not (nil? (re-find pattern (str val)))))

