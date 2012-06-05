(ns tictactoe.test.models.model
  (:use tictactoe.models.model)
  (:use clojure.test)
  (:use noir.util.test)
  (:require [tictactoe.test.models.testdata :as td]))

(deftest get-board-cell-test
  (let [testboard [[\X \- \-]
                   [\- \O \-]
                   [\- \- \X]]]
    (is (get-board-cell testboard 0 0) \X)
    (is (get-board-cell testboard 0 1) \-)
    (is (get-board-cell testboard 1 1) \O)
    (is (get-board-cell testboard 2 2) \X)))

(deftest transposed-board-test
  (doseq [io-pair td/transposed-test-data]
    (is (transposed-board (:input io-pair))
        (:expected-output io-pair))))

(defmacro defboardtest [name winfn positives negatives]
  `(deftest ~name
     (doseq [player# [\X \O]]     
       (doseq [board# (~positives player#)]
         (is (= (~winfn board# player#) true) 
             (str "Player " player# " should win with board " board#)))
       (doseq [board# (~negatives player#)]
         (is (= (~winfn board# player#) false)
             (str "Player " player# " should not win with board " board#))))))

(defboardtest winner-in-rows?-test
              winner-in-rows?
              td/row-win-combinations
              td/no-row-win-combinations)

(defboardtest winner-in-cols?-test
              winner-in-cols?
              td/col-win-combinations
              td/no-col-win-combinations)

(defboardtest winner-in-diagonals?-test
              winner-in-diagonals?
              td/diag-win-combinations
              td/no-diag-win-combinations)

(deftest full-board?-test
  (doseq [player [\X \O]]
    (doseq [board (td/full-boards player)]
      (is (= (full-board? board) true)
          (str "Board should be considered full, but isn't: " board)))
    (doseq [board (td/no-full-boards player)]
      (is (= (full-board? board) false)
          (str "Board should not be considered full, but is: " board)))))

;; exercise: add deftest for function winner?
(deftest winner?-test
  (let [testboard [[\X \O \O]
                   [\- \X \-]
                   [\- \- \X]]]
    (is (winner? testboard \X))
    (is (winner? testboard))))

;; exercise: macro for defining test scenarios which resets game automatically at beginning and end
(defmacro deftestreset [name & body]
  `(deftest ~name
     (with-noir
	     (reset-game!)
	     ~@body
	     (reset-game!))))

;; exercise: refactor scenario1-test using the macro
(deftestreset scenario1-test
  "it should not be possible to choose a cell that is already taken"
  (play! 0 0)
  (is (= (get-board-cell 0 0) \X))
  (play! 0 1)
  (is (= (get-board-cell 0 1) \O))
  (play! 0 2)
  (is (= (get-board-cell 0 2) \X))
  (is (= (get-player) \O))
  (play! 0 0)
  (is (= (get-board-cell 0 0) \X) "value of cell 0 0 should still be X")
  (is (= (get-player) \O) "player should still be O"))

;; exercise: more scenario's
;;       - player X wins
;;       - player O wins
;;       - it's a draw
(deftestreset playerXWins
  (play! 0 0)
  (is (= (get-board-cell 0 0) \X))
  (play! 1 0)
  (is (= (get-board-cell 1 0) \O))
  (play! 0 1)
  (is (= (get-board-cell 0 1) \X))
  (play! 1 1)
  (is (= (get-board-cell 1 1) \O))
  (play! 0 2)
  (is (= (get-board-cell 0 2) \X))
  (is (= (winner?) \X) "Winner should be X"))

(deftestreset playerOWins
  (play! 0 0)
  (is (= (get-board-cell 0 0) \X))
  (play! 1 0)
  (is (= (get-board-cell 1 0) \O))
  (play! 0 1)
  (is (= (get-board-cell 0 1) \X))
  (play! 1 1)
  (is (= (get-board-cell 1 1) \O))
  (play! 2 0)
  (is (= (get-board-cell 2 0) \X))
  (play! 1 2)
  (is (= (get-board-cell 1 2) \O))
  (is (= (winner?) \O) "Winner should be O"))

(deftestreset draw
  (play! 0 0)
  (is (= (get-board-cell 0 0) \X))
  (play! 1 0)
  (is (= (get-board-cell 1 0) \O))
  (play! 0 1)
  (is (= (get-board-cell 0 1) \X))
  (play! 1 1)
  (is (= (get-board-cell 1 1) \O))
  (play! 1 2)
  (is (= (get-board-cell 1 2) \X))
  (play! 0 2)
  (is (= (get-board-cell 0 2) \O))
  (play! 2 0)
  (is (= (get-board-cell 2 0) \X))
  (play! 2 1)
  (is (= (get-board-cell 2 1) \O))
  (play! 2 2)
  (is (= (get-board-cell 2 2) \X))
  (is (= (full-board?) true) "Board should be full")
  (is (= (winner?) nil) "Should be a draw")) 