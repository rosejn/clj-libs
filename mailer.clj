(ns mailer
  (:import
     (org.apache.commons.mail SimpleEmail)
     (javax.mail.internet InternetAddress MimeMessage)
     (javax.mail Session Transport Message Authenticator PasswordAuthentication)
     java.util.Properties))

(def *smtp-config* {})

(defn make-authenticator []
  (proxy [Authenticator] []
    (getPasswordAuthentication [] 
                               (new PasswordAuthentication 
                                    (*smtp-config* :login)
                                    (*smtp-config* :password)))))

; TODO: Apply reasonable defaults for things like ssl support, port number etc...
(defn with-smtp-config* [config func]
  (binding [*smtp-config* config]
    (func)))
  
(defmacro with-smtp-config [config & body]
  `(with-smtp-config* ~config (fn [] ~@body)))

(defn send-mail [to from subject body]
  (let [mail (new SimpleEmail)
        recipients (cond 
                     (string? to) [to]
                     (seq? to)     to
                     true (throw 
                            (IllegalArgumentException.  
                              "Recipient must be a string email address or a sequence of email address strings.")))]
    (doto mail
      (.setHostName (*smtp-config* :server))
      (.setFrom from)
      (.setSubject subject)
      (.setMsg body)
      (.setAuthenticator (make-authenticator))
      (.setSSL true)
      (.setTLS true)
      (.setSslSmtpPort (*smtp-config* :port)))
  (doseq [addr recipients]
    (.addTo mail addr))
  (.send mail)))
