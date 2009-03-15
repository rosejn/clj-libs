(ns compojure-extra.action
  (:use compojure.html))

(defonce CONTROLLER-BASE (ref nil))

(defn action-ns 
"Specify the base namespace for looking up controllers."
  [ns]
  (dosync 
    (ref-set CONTROLLER-BASE (str ns "."))))

(defn- keyword-to-str 
"Convert a keyword to a string without the ':' prefix."
  [k]
  (.substring (str k) 1))

(defn- keyword-to-sym 
"Convert a keyword to a symbol without the ':' prefix." 
  [k]
  (symbol (keyword-to-str k)))

(defn make-str 
"Convert keyword argument to string if necessary."
  [val]
  (if (keyword? val)
    (keyword-to-str val)
    val))

(defn make-sym [val]
"Convert keyword argument to symbol if necessary."
  (if (keyword? val)
    (keyword-to-sym val)
    (symbol val)))

(defn action-handler 
"Lookup a handler function given the controller and action names." 
  [controller action]
  (let [controller (make-str controller)
        action (if action 
                 (make-sym action) 
                 'index)
        handler (get 
                  (ns-publics (symbol (str @CONTROLLER-BASE controller))) 
                  action)]
    handler))

(defn action 
"Run an action handler given the controller and optional action name.  If no action was specified then it will call the 'index' action."  
  [controller & [action]]
  (let [handler (action-handler controller action)]
    (if (nil? handler)
      (throw (Exception. (str "Invalid controller or action requested: " controller action)))
      (handler))))

(defn action-url 
"Returns the url to be used for a controller and optional action."
  [controller & [action]]
  (let [controller (make-str controller)]
    (if action
      (str "/" controller "/" (make-str action))
      (str "/" controller))))

(defn build-args 
"Inspects an action handler function and builds an argument list to pass it the correct data."
  [handler, vars]
  (reverse (reduce 
             (fn [args arg]
               (let [arg-key (keyword (str arg))]
                 (if (contains? vars arg-key)
                   (conj args (get vars arg-key))
                   (throw (Exception.  (str "Unknown parameter in action handler: " arg))))))
             nil
             (first (get (meta handler) :arglists)))))

(defn ajax? 
"Checks the HTTP header to determine whether the request was made as an AJAX request using the XMLHttpRequest object in javascript."  
  [headers]
  (= "XMLHttpRequest" (:x-requested-with headers)))

(defmacro ajax-html 
"Used to wrap actions so the (html ...) function will be called when necessary.  Need to think through this in more depth though..."  
  [headers & body]
  `(if (ajax? ~headers)
     (html ~@body)
     ~@body))

(defmacro base-action 
"This is used in the servlet route-table to lookup actions and call them with the correct arguments."  
  [controller & [action]]
  `(let [vars# {:context  ~'context 
                :method   ~'method  
                :url      ~'url     
                :path     ~'path    
                :params   ~'params  
                :headers  ~'headers 
                :mimetype ~'mimetype
                :session  ~'session 
                :cookies  ~'cookies}
         handler# (action-handler ~(do controller) ~(do action))]
     (ajax-html ~'headers
       (apply handler# (build-args handler# vars#)))))
