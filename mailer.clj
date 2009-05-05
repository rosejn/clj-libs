(ns mailer
  (:import
     (org.apache.commons.mail SimpleEmail DefaultAuthenticator)))

(def *default-config* {:port 25
                       :tls false
                       :ssl false
                       })

(defn set-default-config! [config]
  (def *default-config* config))

; Send an email message to one or more recipients.
;
; Arguments: 
;  config - A map specifying smtp related properties 
;   :server   [string]
;   :login    [string]
;   :password [string]
;   :port     [int]     Port number
;   :tls      [boolean] Use tls encryption for authentication?
;   :ssl      [boolean] Use an ssl transport?
;   :ssl-port [int]     SSL port
;
;  msg - An email map, or a seq (Vector, List...) of email maps. 
;    :to       [string or a seq of strings]
;    :from     [string]
;    :subject  [string] 
;    :body     [string]
;
; Example:
; 
; (def *smtp-config* {:server   "smtp.gmail.com"
;                     :login    "me@gmail.com"
;                     :password "my-password"
;                     :tls true})
;
; (send-mail *smtp-config*
;            {:to "alan.turing@computer.com"
;             :from "me@gmail.com"
;             :subject "Hey Al!"
;             :body "So I was thinking about that machine you were talking
;             about..."})

; TODO: Add an asynchronous option that will return a Future and then send the
; mail in a separate mail sender thread that operates on a queue of pending
; messages.

(defn send-mail [config mail]
  (let [config (merge *default-config* config)
        {:keys [server login password port ssl tls]} config
        mail (cond 
               (map? mail) [mail]
               (seq? mail) mail
               true (throw
                      (IllegalArgumentException. 
                        "The mail argument must be either a map describing a single message or a sequence of such maps.")))]

    (doseq [msg mail] 
      (let [{:keys [to from subject body]} msg
            mail (new SimpleEmail)
            recipients (cond 
                         (string? to) [to]
                         (seq? to)     to
                         true (throw 
                                (IllegalArgumentException.  
                                  "Recipient must be a string email address or a sequence of email address strings.")))]
        (doto mail
          (.setHostName server)
          (.setFrom from)
          (.setSubject subject)
          (.setMsg body)
          (.setAuthenticator (DefaultAuthenticator. login password))
          (.setSmtpPort port)
          (.setSSL ssl)
          (.setTLS tls))

        (if (contains? config :ssl-port)
          (.setSslSmtpPort mail (str (config :ssl-port))))

        (doseq [addr recipients]
          (.addTo mail addr))
        (.send mail)))))

