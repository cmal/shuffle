(ns shuffle.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn shuffle1
  [v]
  "shuffle1: random choose one of the remaining vector
and add to the result vector's end"
  (loop [res       []
         remaining v]
    (if (= (count v) (count res))
      res
      (let [r (nth remaining (rand-int (count remaining)))]
        (recur (conj res r)
               (remove #{r} remaining))))))

(defn shuffle2
  [v]
  "shuffle2: take 1 of the remaining vector and
add to a random position of the result vector"
  (loop [res []
         n   0]
    (if (= n (count v))
      res
      (let [e (nth v n)
            r (rand-int (inc n))]
        #_(println n r (take r res) [e] (drop r res))
        (recur (vec (concat (take r res) [e] (drop r res)))
               (inc n))))))

(defn test-shuffle-pos
  [f v]
  "test whether the positions of result vector's elements
all takes a possiblity of (/ 1 n) at every distinct position
result: the list of times
that every item in v falls at position 0, 1, 2, ... (count v)"
  (let [tests   10000
        results (for [n (range tests)]
                  (f v))]
    (map
     (fn [n] (for [item v]
               (count (filter #{item} (map #(nth % n) results)))))
     (range (count v)))))

(test-shuffle-pos shuffle1 [1 2 3 4 5])
;; ((2033 1873 1995 2065 2034) (1959 2111 1940 1971 2019) (1984 1984 2065 1971 1996) (2020 1969 1994 2035 1982) (2004 2063 2006 1958 1969))

(test-shuffle-pos shuffle2 [1 2 3 4 5])
;; ((1992 1979 2014 1974 2041) (2039 1949 2010 2000 2002) (1964 2016 2025 2030 1965) (2052 2033 1950 1984 1981) (1953 2023 2001 2012 2011))

;; It seems shuffle1 and shuffle2 both make every item fairly falls at every position.

(defn test-shuffle-item-distance
  [f v]
  "test whether the mutual distance of result vector's elements
all takes a possibility of (/ (combination n 2) n)
result: the list of times that every combination occurs"
  (let [tests   10000
        results (for [n (range tests)]
                  (f v))
        len     (count v)]
    (->>
     (map #(for [x     %
                 y     %
                 :when (< x y)]
             [(keyword (str x "-" y))
              (mod (- (.indexOf % x) (.indexOf % y))
                   len)]) results)
     (apply concat)
     (group-by identity)
     (map (comp count val)))))

(test-shuffle-item-distance shuffle1 [1 2 3 4 5])
;; (2432 2467 2493 2575 2568 2465 2443 2562 2533 2429 2454 2535 2515 2540 2560 2563 2526 2484 2477 2522 2506 2459 2448 2503 2491 2500 2431 2534 2457 2574 2555 2587 2496 2414 2475 2502 2488 2516 2408 2513)

(test-shuffle-item-distance shuffle2 [1 2 3 4 5])
;; (2516 2503 2616 2526 2567 2558 2506 2513 2475 2459 2534 2523 2545 2416 2468 2506 2384 2421 2489 2541 2471 2459 2437 2526 2462 2483 2413 2581 2575 2534 2494 2539 2532 2431 2477 2496 2566 2503 2487 2468)

;; It seems both shuffle1 and shuffle2 can dispatch every items of v
;; to get a fair mutual distance.

(time (doseq [n (range 10000)] (shuffle1 (range 100))))
"Elapsed time: 9087.917178 msecs"
(time (doseq [n (range 10000)] (shuffle2 (range 100))))
"Elapsed time: 13072.131968 msecs"

(time (do (shuffle1 (range 10000)) nil))
"Elapsed time: 7937.873068 msecs"
(time (do (shuffle2 (range 10000)) nil))
"Elapsed time: 12249.9654 msecs"

;; CONCLUSION:
;; Due to Monte Carlo simulation, we can believe that
;; shuffle1 and shuffle2 both can do a random shuffle work.
;; It seems shuffle1 always do a better job than shuffle2.
;; That may due to shuffle1 do more rand, but shuffle2 do
;; more list manipulations.
