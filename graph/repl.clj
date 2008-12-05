(import 
  '(java.io InputStreamReader OutputStreamWriter)
  '(clojure.lang LineNumberingPushbackReader))


(def *files* ["internal/internal.clj", "graph.clj"])

(defn print-prompt []
  (print (str (ns-name *ns*)  "=> "))
  (flush))

(defn generic-repl [in-stream out-stream]
  "runs a repl that reads from in-stream and writes to out-stream until eof"
  (binding [*ns* (create-ns 'user)
            *warn-on-reflection* false
            *out* (new OutputStreamWriter out-stream)]
    (let [eof (new Object)
          reader (new LineNumberingPushbackReader (new InputStreamReader in-stream))]
      (print-prompt)
      (loop [next-form (read reader false eof)]
        (when-not (= next-form eof)
          (prn (eval next-form))
          (print-prompt)
          (recur (read reader false eof)))))))

(defn repl []
  "Creates a repl using System.in and System.out"
  (generic-repl (. System in) (. System out)))

(defn reload []
  (doseq [file *files*] 
    (println "Loading file: " file)
    (load-file file)))

(repl)
