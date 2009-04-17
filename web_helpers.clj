(ns web-helpers
  (:import (java.util Date))
  (:use clojure.contrib.json.write compojure.html))

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
                 true {})
        params (merge {:class "ajax-link"} params)]
    (link-to-function txt (js-load-update url target params))))

(defn js-post-update [form-id url target]
  (str "$.post(\"" url "\", $(\"" form-id "\").serialize(), 
          function(data) {
            devo.log(\"Post returned data: \" + data);
            $(\"" target "\").html(data);
          });"))

(defmacro ajax-form-to [url target & body]
  ;`[:form {:method "POST" :action ~url
  `[:form {:onsubmit (js-remote-post ~url ~target)}
   ~@body])

(defn ajax-submit-button [txt form-selector url target]
  (link-to-function txt (js-post-update form-selector url target) 
                    {:class "ajax-link"}))

