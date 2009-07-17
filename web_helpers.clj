(ns web-helpers
  (:import (java.util GregorianCalendar))
  (:use clojure.contrib.json.write compojure.html
     compojure.encodings))

(defn seconds [secs] secs)
(defn minutes [mins] (seconds (* 60 mins)))
(defn hours [hours] (minutes (* 60 hours)))
(defn days [days] (hours (* 24 days)))
(defn weeks [weeks] (days (* 7 weeks)))
(defn months [months] (days (* 30 months)))

(defn cookie
  "Return a Set-Cookie header based on either, a name and value, 
  or a cookie map:
  {:name \"my-cookie\"       
   :value \"data-goes-here\"
   :expires (days 1)                ; optional
   :comment \"special user data\"   ; optional
   :path \"foo/bar\"                ; optional
   :secure true                     ; optional
  }"
  ([name value]
   (cookie {:name name :value value}))
  ([cookie]
   (let [{:keys [name value secure]} cookie
         attrs (-> cookie (dissoc :name) (dissoc :value) (dissoc :secure))
         attr-str (reduce 
                    (fn [res [attr val]] 
                      (str res "; " 
                           (urlencode (clojure.core/name attr)) 
                           "=" 
                           (urlencode val)))
                    ""
                    attrs)
         attr-str (if secure
                    (str attr-str "; secure")
                    attr-str)
         cookie-str (str (urlencode name) "=" (urlencode value)
                         attr-str)]
     {:headers {"Set-Cookie" cookie-str}})))

(defn js-on-ready [code]
  (javascript-tag 
    (str "$(document).ready(function(){\n" code "});")))

(defn js-focus-input [& [which-input]]
  (js-on-ready 
    (if which-input
      (str "devo.focus_input(\"" which-input "\");")
      (str "devo.focus_input();"))))

(defn js-toggle-button [button target]
  (js-on-ready
    (str "devo.toggle_button(\"" button "\", \"" target "\");")))

(defn js-editable [container url & [extra-params]]
  (js-on-ready 
    (str  
      "$('" container " .editable').editable('" url "', {
         indicator : 'Saving...',
         tooltip   : 'Click to edit...'"
      (if extra-params
        (str ", submitdata: " (json-str extra-params)))
     "});

     $('" container " .editable-area').editable('" url "', { 
         indicator : 'Saving...',
         tooltip   : 'Click to edit...',
         type      : 'textarea',
         cancel    : 'Cancel',
         submit    : 'OK',
         rows      : 10,
         cols      : 40"
      (if extra-params
        (str ", submitdata: " (json-str extra-params)))
     "});")))

(defn js-file-uploader [target url]
  (javascript-tag 
    (str "$(document).ready(function() {
         new AjaxUpload('" target "', {action: '" url "'});
         });")))

(defn mail-to [addr content]
  [:a {:href (str "mailto:" addr)} content])

(defn link-to-function [txt js-fun & [options]]
  (let [options (or options {})]
    [:a (merge options {:onclick js-fun}) txt]))

(defn- js-load-update [url target params]
  (str "$(\"" target "\").load(\"" url "\", "
       (json-str params) ", function() {
         devo.log(\"ajax load complete...\");
       });"))

(defn ajax-link-to 
"Params is either a map or paired arguments to the function:
  (ajax-link-to txt url tgt {:a 1 :b 2})
  (ajax-link-to txt url tgt :a 1 :b 2)
"
  [txt url target & params]
  (let [params (cond 
                 (associative? (first params)) (merge {} (first params))
                 (even? (count params)) (apply hash-map params)
                 true {})]
    (link-to-function txt 
                      (js-load-update url target params)
                      {:class "ajax-link"})))

(defn js-post-update [form-id url target]
  (str "$.post('" url "', $('" form-id "').serialize(), 
          function(data) {
            devo.log('Post returned data: ' + data);
            $('" target "').html(data);
          });"))

(defmacro ajax-form-to [url target & body]
  ;`[:form {:method "POST" :action ~url
  `[:form {:onsubmit (js-remote-post ~url ~target)}
   ~@body])

(defn ajax-submit-button [txt form-selector url target]
  (link-to-function txt (js-post-update form-selector url target) 
                    {:class "ajax-link"}))

