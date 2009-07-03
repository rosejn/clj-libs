(ns vote)

(defstruct vote-result :winner :count)

(defn- tally-votes [votes]
  (reduce (fn [tally vote] 
            (assoc tally vote (inc (get tally vote 0))))
          {} votes))

(defn simple-plurality 
 "One vote per voter, and the reciever of the most votes wins.
   http://en.wikipedia.org/wiki/Plurality_voting_system

  votes is a seq of candidate keys where each key represents a single vote for a single candidate.  For example, use the name of each candidate as the key.
 "
  
  [votes] 
   (let [tally (tally-votes votes)
         winner (apply max-key 
                       (fn [[name count]] count) 
                       (seq tally))
         winner-key (first winner)]
       (struct vote-result winner-key tally)))

(defn drop-candidate 
  "Drop all votes for a given candidate by removing them from all ballots."
  [votes candidate]
  (filter (complement empty?)
          (map (fn [ballot] 
                 (filter #(not (= candidate %1)) ballot))
               votes)))

(defn instant-runoff 
  "Voters rank candidates in order.  The votes are tallied by counting the first preference of each voter, and if any candidate receives a majority then they win.  If there is no majority winner, then the candidate with the fewest votes is eliminated, and all voters who had previously voted for that candidate will now have their first place votes dropped and their second place votes counted.  Using the new count the process is repeated iteratively until one candidate receives a majority.
  http://en.wikipedia.org/wiki/Instant_Runoff_Voting

  votes is a seq of vectors, where each voter's preference list is a sorted seq of candidate keys.
 "
  [votes]
  (let [num-voters (count votes)
        tally (tally-votes (map first votes))
        sorted-tally (sort-by 
                       (fn [[candidate votes]] votes) 
                       #(> %1 %2) 
                       tally)
        winner (first sorted-tally)
        has-majority (> (second winner) (/ num-voters 2))
        loser-key  (first (last sorted-tally))]
    (if has-majority
      (struct vote-result (first winner) tally) ; Winner has majority
      (recur (drop-candidate votes loser-key)))))

;Should be C
(def votes (seq "ABCABCABCCCBBAAABABABCCCCCCCCCCCCCA"))

(defn test-plurality []
  (simple-plurality votes))

; Should be C
(def runoff-votes (concat
                    (take 14 (repeat [:a :b :c :d]))
                    (take 13 (repeat [:b :c :d :a]))
                    (take 12 (repeat [:c :d :b :a]))
                    (take 10 (repeat [:d :c :b :a]))))

(defn test-runoff []
  (instant-runoff runoff-votes))
