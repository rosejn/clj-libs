(ns mailer
  (:import
     (org.apache.commons.mail SimpleEmail)
     (javax.mail.internet InternetAddress MimeMessage)
     (javax.mail Session Transport Message Authenticator PasswordAuthentication)
     java.util.Properties)
  (:require config))

(defn make-authenticator []
  (proxy [Authenticator] []
    (getPasswordAuthentication [] 
                               (new PasswordAuthentication 
                                    (config/value :mail-login)
                                    (config/value :mail-password)))))

(defn send-mail [to from subject body]
  (let [mail (new SimpleEmail)
        recipients (cond 
                     (string? to) [to]
                     (seq? to)     to
                     true (throw 
                            (IllegalArgumentException.  
                              "Recipient must be a string email address or a sequence of email address strings.")))]
    (doto mail
      (.setHostName (config/value :mail-smtp-host)
                    (.setFrom from)
                    (.setSubject subject)
                    (.setMsg body)
                    (.setAuthenticator (make-authenticator))
                    (.setSSL true)
                    (.setTLS true)
                    (.setSslSmtpPort "465")))
      (doseq [addr recipients]
        (.addTo mail addr))
      (.send mail)))
