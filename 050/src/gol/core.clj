(ns gol.core
  (:gen-class))

(defn get-cell-state
  "get true/false in map - treat off map as false"
  [x y board]
  (nth (nth board x []) y false)
  )

(defn count-live-neighbors
  "get a count of live neighbors for game of life"
  [x y board]
  (count
   (filter #(= % true)
           (into []
                 (map #(apply get-cell-state (conj % board))
                      [
                       [ (dec x) (dec y) ]
                       [ (dec x) y ]
                       [ (dec x) (inc y) ]
                       [ x (dec y)]
                       [ x (inc y)]
                       [ (inc x) (dec y)]
                       [ (inc x) y]
                       [ (inc x) (inc y)]
                       ]
                      )
                 )
           )
   )
  )

(defn get-next-state
  [x y board]
  (case (count-live-neighbors x y board)
    0 false
    1 false
    2 true
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

(def startboard
  [
    [0 1 0 0 0]
    [0 0 1 0 0]
    [1 1 1 0 0]
    [0 0 0 0 0]
    [0 0 0 0 0]
  ]
  )

(defn convert-board [board]
  "Turn the starting board into bools"
  (into
   []
   (for [row board]
     (into
      []
      (for [cell row]
        (not= cell 0)
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
        (get-next-state x y board)
        )
      )
     )
   )
  )

(defn step [board]
  (render-board board)
  (Thread/sleep 1000)
  (get-next-board board)
  )

(defn -main
  [& args]
  (step
   (step
  (step (convert-board startboard))
 ))
  )

(-main)
