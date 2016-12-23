(ns scurve-scores.core
  "Returns game stats using .csv file"
  (require [clojure.string :as string])
  (:gen-class))

;; data is the expected value of records in -main function. It is here for testing only
;; TODO create tests
(def data '({:initials "NHD", :level "001", :time 36, :l/w "W"} {:initials "PET", :level "002", :time 15, :l/w "W"} {:initials "NHD", :level "002", :time 20, :l/w "L"}))

(defn unique-vals
  "Returns a set containing all values associated with key in xs, a seq of maps"
  [key xs]
  (into #{} (map key xs)))

(defn time-in-game
  "Returns the total time the plyr has spent in-game"
  [plyr xs]
  (let [filt (fn [m] (= plyr (:initials m)))]
   (->> xs
    (filter filt)
    (map :time)
    (reduce +))))

;; TODO can you make a factory or generalized fn for time-in-game and best-score?
(defn best-score
  "Returns the lowest :time value for lvl"
  [lvl xs]
  (let [filt (fn [m] (= lvl (:level m)))]
    (->> xs
     (filter filt)
     (map :time)
     (apply min))))


;; TODO collect-times and collect-scores use different implementation, they should be the same
(defn collect-times
  "Returns a seq of vecs with format [initials time-in-game]"
  [ids records func]
  (sort func (map #(identity [% (time-in-game % records)]) ids)))

(defn collect-scores
  "Returns a seq of vecs with format [level best-score], sorted by level name"
  [records]
  (sort-by first
    (reduce (fn [v lvl] (conj v [lvl (best-score lvl records)]))
            []
            (unique-vals :level records))))



(defn line->plyr-record
  "Returns a map representing a player record"
  [line]
  (let [[initials level time lw] (string/split line #",")]
   {:initials initials
    :level level
    :time (Integer. time)
    :l/w lw}))

;; TODO convert seconds to hrs and mins
;; TODO allow user to identify input via args
(defn -main
  "Returns game stats using .csv file"
  [& args]
  (let [records (->> "resources/scores.csv"
                (slurp)
                (string/split-lines)
                (rest)
                (map line->plyr-record))
        players (unique-vals :initials records)
        levels (unique-vals :lvl records)]

        (println "Total seconds played")
        (doseq [line (collect-times players records #(compare (last %2) (last %1)))]
          (println (first line) "\t" (last line)))

      ;; TODO format output for clarity
        (println "Top scores")
        (doseq [line (collect-scores records)]
          (println (first line) "\t" (last line)))))
