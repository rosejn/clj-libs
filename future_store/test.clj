(ns future-store.test
  (:use 
     jlog
     clojure.contrib.test-is
     future-store
     (future-store raw manager)))

(defmacro test-store 
"Executes body within the context of a store named \"test-store\" that will be automatically deleted when body completes or an exception occurs."  
  [& body]
  `(try 
     (with-store "test-store"
       ~@body)
     (finally (delete-store "test-store"))))


(defmacro test-manager
"Executes body within the context of a store named \"test-store\" that will be automatically deleted when body completes or an exception occurs."  
  [& body]
  `(try 
     (view-store "test-store")
       (do 
         ~@body)
     (finally (do
                (manager-stop)
                (delete-store "test-store")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Raw tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest get-set-props []
  (test-store
    (set-property (root-node) "foo" 42)
    (is (= 42 (get-property (root-node) "foo")))
    (info (str "Properties: " (get-property-keys (root-node))))
    (let [n (add-node {"bar" 1234})]
      (info (str "Properties: " (get-property-keys n)))
      (is (= 1234 (get-property n "bar"))))))

(deftest add-remove []
  (test-store
    (let [n (add-node)
          id (get-id n)]
      (is (= id (get-id (find-node id))))
      (remove-node n)
      (is (nil? (find-node id))))))

(defn n-children [parent n label]
  (info "(n-children " (get-id parent) " " n " " label ")")
  (if (zero? n)
    parent
    (do
      (link-new parent label)
      (recur parent (- n 1) label))))

(deftest simple-query []
  (test-store
    (let [root (root-node)]
      (in-tx 
        (n-children 
          (link-new (link-new (link-new (link-new root 
                                                  :foo) :foo) :foo) :foo {"value" 42})
          5
          :bar)
        (success))
      (let [tgt (first (path-query root [:foo :foo :foo :foo]))
            val (.getProperty tgt "value")
            edge-count (count (out-edges tgt :bar))
            spread-count (count (path-query root [:foo :foo :foo :foo :bar]))]
        (info "simple-query tgt: " tgt)
        (is (= 42 val))
        (is (= 5 edge-count))
        (is (= 5 spread-count))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defview post)
(deftest test-view []
         (test-manager
           (let [base    (manager-do #(view-root "post"))
                 counter (manager-do #(get-property base :id-counter))]
             (is (not (nil? base)))
             (is (= 0 counter))
             (let [new-post (post/create {:title "Peppernoten for life"
                                          :text  "Here we go..."})
                   counter (manager-do #(get-property base :id-counter))
                   new-id  (:id new-post)
                   text    (:text new-post)
                   post    (post/find 0)]
               (is (= 0 new-id))
               (is (= 1 counter))
               (is (= "Here we go..." text))
               (is (= (:node new-post) (:node post)))))))

(defview bam)
(deftest test-view-find []
         (test-manager
           (dotimes [i 100]
             (bam/create {:num (* 10 i)}))
             (is (= 20 (:num (bam/find 2))))))

(defview bom)
(deftest test-view-delete []
         (test-manager
           (bom/create {:val 1})
           (bom/create {:val 2})
           (is (= 1 (:val (bom/find 0))))
           (bom/delete 0)
           (is (nil? (bom/find 0)))
           (bom/delete (bom/find 1))
           (is (nil? (bom/find 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manager tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                                        
;; TODO: write some tests that slam the manager from a bunch of threads and
;; test out weird border cases so we know it really does it's job.
                                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Index tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
(defn child-by-property [parent label property value]
  (let [children (out-nodes parent label)]
    (first (filter #(= value (get-property % property)) children))))

(deftest index-vs-raw []
  (test-store
    (let [root (root-node)
          size 100]
      (dotimes [i size]
        (link-new root :foo {:counter i}))
      (let [index (index-property root :counter :foo)
            num-trials 10
            trial-seq (for [i (range num-trials)] (rand-int size))]
        (doseq [val trial-seq]
          (let [ival (lookup index val)
                rval (child-by-property root :foo :counter val)]
            (info "val: " val " ival: " (get-property ival :counter) 
                  " rval: " (get-property rval :counter))
            (is (= ival rval))))))))

(def index-speed-test []
  (test-store 
    (let [root (root-node)
          size 1000]
      (dotimes [i size] (link-new root :foo {:counter i}))

      (let [index (index-property root :counter :foo)
            num-trials 100
            trial-seq (for [i (range num-trials)] (rand-int size))
            raw-time (time (doseq [val trial-seq]
                             (child-by-property root :foo :counter val)))
            index-time (time (doseq [val trial-seq]
                               (lookup index val)))]
        (println "Raw time: " raw-time "\nIndex time: " index=-time)))))
)                                                       

(defn fs-tests [] 
  (run-tests (find-ns 'future-store.test))
  (delete-store "test-store"))
