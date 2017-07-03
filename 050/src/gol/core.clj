(ns gol.core
  (:gen-class))

(def startboard
  "
  .....
  .....
  .XXX.
  .....
  .....
  "
  )

(defn convert-board
  "take board as string and convert to nested vector bools"
  [input]
  (vec
   (map (fn [instring] (vec (into [] (map #(not= \. %) instring))))
        (filter not-empty
                (map clojure.string/trim
                     (clojure.string/split input #"\n")
                     )
                )
        )
   )
  )


(defn get-surrounding-cells
  "return the surrounding cells from x y"
  [x y]
  [
   [ (dec x) (dec y) ]
   [ (dec x)  y ]
   [ (dec x) (inc y) ]
   [      x  (dec y)]
   [      x  (inc y)]
   [ (inc x) (dec y)]
   [ (inc x)  y]
   [ (inc x) (inc y)]
   ]
  )

(defn get-cell-state
  "get true/false in map - treat off map as false"
  [x y board]
  (nth (nth board x []) y false)
  )

(defn count-live-neighbors
  "get a count of live neighbors for game of life"
  [x y board]
  (->>
   (get-surrounding-cells x y)
   (map #(apply get-cell-state (conj % board)))
   (filter #(= % true))
   count
   )
  )

(defn get-next-state
  "get the next state"
  [x y current board]
  (case (count-live-neighbors x y board)
    0 false
    1 false
    2 (and current true)
    3 true
    false
    )
  )

(defn render-board [board]
  (println
  (clojure.string/join "\n"
        (for [row board]
          ; apply to not get lazy seq signature back as string
          (apply str
               (for [cell row] (if cell "x" " "))
               )
          )
       )
  )
  )


(defn get-next-board
[board]
  (into
   []
   (for [x (range 0 (count board))]
     (into
      []
      (for [y (range 0 (count board))]
        (get-next-state x y (get (get board x) y)  board)
        )
      )
     )
   )
  )

(defn step [board]
  (print (str (char 27) "[2J")) ; clear screen
  (print (str (char 27) "[;H")) ; move cursor to the top left corner of the screen
  (render-board board)
  (Thread/sleep 500)
  (get-next-board board)
  )

(defn run [board]
  (recur (step board)
  )
  )

(defn -main
  [& args]
  (run
   (convert-board startboard)
   )
  )
