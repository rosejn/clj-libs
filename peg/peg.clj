(ns peg
  (:import java.util.Scanner))

(def *input* nil)

(defn- pattern? [p]
  (= java.util.regex.Pattern (class p)))

(def WHITESPACE #"^\s*")

(defn- forward [n]
  (set! *input* (.substring *input* n)))

(defn- trim-whitespace []
  (set! *input* (.substring *input* (count (re-find WHITESPACE *input*)))))

(defn- match [expr]
  "Match an expression, forwarding the input on each valid match."
  (let [saved *input*]
    (trim-whitespace)
    (let [res (expr)]
      (if (nil? res)
        (do 
          (set! *input* saved)
          res)
        res))))

(defn expr [re]
  "A basic expression is either a string or a regex"
  (let [re (cond
             (pattern? re) (re-pattern (str "^" (.pattern re)))
             (string? re) (re-pattern (str "^" re))
             true (throw Exception "expr takes either a string or a regular expression pattern"))]
    (fn [] 
      ;(println "expr: " (.pattern re))
      ;(println "input: " *input*)
      (let [res (re-find re *input*)]
        (cond
          (nil? res)    nil
          (vector? res) (do 
                          (forward (count (first res)))
                          (first res))
          (string? res) (do 
                          (forward (count res)) 
                          res))))))

(defn expr-seq [& exprs]
  "A sequence of expressions that must match in order."
  (fn []
    (loop [exprs    exprs
           matches  nil]
      (if (nil? exprs)
        (reverse matches)
        (let [res (match (first exprs))]
          (if (not (nil? res))
            (recur (next exprs) (conj matches res))))))))

(defn expr-choice [& exprs]
  "A seq of choices, tried in order until the first matching expression."
  (fn []
    (loop [exprs exprs]
      (if (not (nil? exprs))
        (let [res (match (first exprs))]
          (if (nil? res)
            (recur (next exprs))
            res))))))

(defn expr* [expr]
  (fn []
    (loop [matches nil]
      (let [res (match expr)]
        (if (nil? res)
          (reverse matches)
          (recur (conj matches res)))))))

(defn expr+ [expr]
  (let [repeater (expr* expr)]
    (fn []
      (let [res (repeater)]
        (if (empty? res)
          nil
          res)))))

(defn expr? [expr]
  (fn []
    (let [res (match expr)]
      (if (nil? res)
        true
        res))))

(defn pred& [expr]
  (fn []
    (let [save *input*
          res (match expr)]
      (set! *input* save)
      (if (not (nil? res))
        true
        nil))))

(defn pred! [expr]
  (fn []
    (let [save *input*
          res (match expr)]
      (set! *input* save)
      (if (nil? res)
        true
        nil))))

(defn eos []
  (if (empty? *input*)
    true
    nil))

(defn grammar [expr]
  (fn [input]
    (binding [*input* input]
      (expr))))

(defn parser [expr fun]
  (fn []
    (let [res (expr)]
      ;(println "parser res: " res)
      (if (seq? res)
        (apply fun res)
        (fun res)))))

(comment
(def test-input "raining cats cats dogs cats")
(def raining (expr #"raining"))
(def cats (expr #"cats"))
(def dogs (expr #"dogs"))

(def rain-grammar (grammar raining))
(print "rain grammar: ")
(println (rain-grammar test-input))

(def seq-grammar (grammar (expr-seq raining cats)))
(print "seq grammar: ")
(println (seq-grammar test-input))

(def choice-grammar (grammar (expr-choice dogs cats raining)))
(print "choice grammar: ")
(println (choice-grammar test-input))

(def rep-grammar (grammar (expr-seq raining
                                    (expr*
                                      (expr-choice dogs cats)))))
(print "repeat grammar: ")
(println (rep-grammar test-input))

(def num-expr (expr #"(-)?\d+"))
(def num-grammar (grammar num-expr))
(print "num grammar: ")
(println (num-grammar "234"))
  )

;; Calculator
(def NUMBER (parser (expr #"(-)?\d+") 
                    (fn [n] (println "num: " n) (Integer. n))))
(def ADD    (parser (expr-seq NUMBER (expr #"\+") NUMBER)
                    (fn [a plus b] (println "add: " a plus b) (+ a b))))
(def SUB    (parser (expr-seq NUMBER (expr #"\-") NUMBER)
                    (fn [a minus b] (- a b))))
(def MUL    (parser (expr-seq NUMBER (expr #"\*") NUMBER)
                    (fn [a times b] (* a b))))
(def DIV (parser (expr-seq NUMBER (expr #"\/") NUMBER)
                    (fn [a div b] (/ a b))))
(def calc-grammar (grammar ADD))
(print "calc grammar: ")
(println (calc-grammar "3 + 5"))

;; Tests
(use 'clojure.contrib.test-is)

(deftest test-seq []
         (let [aaa (expr #"aaa")
               grm (grammar (expr-seq aaa aaa))]
           (are (= _1 _2)
                '("aaa" "aaa") (grm "aaa aaa aaa bbb")
                nil (grm "aaa bbb")
                nil (grm "bbb"))))

(deftest test-choice []
         (let [aaa (expr #"aaa")
               bbb (expr #"bbb")
               grm (grammar (expr-choice aaa bbb))]
           (are (= _1 _2)
                "aaa" (grm "aaa aaa aaa bbb")
                "aaa" (grm "aaa bbb")
                nil   (grm "ccc aaa bbb")
                "bbb" (grm "bbb"))))

(deftest test-expr* []
         (let [aaa (expr #"aaa")
               grm (grammar (expr* aaa))]
           (are (= _1 _2)
                '("aaa" "aaa" "aaa") (grm "aaa aaa aaa bbb")
                '("aaa") (grm "aaa bbb")
                '() (grm "bbb"))))

(deftest test-expr+ []
         (let [aaa (expr #"aaa")
               grm (grammar (expr+ aaa))]
           (are (= _1 _2)
                '("aaa" "aaa" "aaa") (grm "aaa aaa aaa bbb")
                '("aaa") (grm "aaa bbb")
                nil (grm "bbb"))))

(deftest test-expr? []
         (let [aaa (expr #"aaa")
               grm (grammar (expr? aaa))]
           (are (= _1 _2)
                "aaa" (grm "aaa aaa aaa bbb")
                "aaa" (grm "aaa bbb")
                true  (grm "bbb"))))

(deftest test-pred& []
         (let [aaa (expr #"aaa")
               grm (grammar (pred& aaa))]
           (are (= _1 _2)
                true (grm "aaa aaa aaa bbb")
                true (grm "aaa bbb")
                nil  (grm "bbb"))))

(deftest test-pred! []
         (let [aaa (expr #"aaa")
               grm (grammar (pred! aaa))]
           (are (= _1 _2)
                nil (grm "aaa aaa aaa bbb")
                nil (grm "aaa bbb")
                true  (grm "bbb"))))

(run-tests)
