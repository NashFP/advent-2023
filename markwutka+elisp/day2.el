;;; This time, instead of operating directly on the buffer, we
;;; make a read-lines function that reads in the buffer as a
;;; list of lines and process it from there.
;;;
;;; This one has a much more functional style.

;;; Read in a file as a list of lines
(defun read-lines (file)
  (interactive)
  (with-temp-buffer
    ;;; Load the file
    (insert-file-contents file)
    (let ((lines '()))
      ;;; Match each line
      (while (re-search-forward "\\(.*\\)[\n]" nil t)
	;;; Okay, this isn't very functional. Anyway, we build
	;;; the list in reverse
	(setq lines (cons (match-string 1) lines)))
      ;;; ... and then reverse it at the end
      (reverse lines))))


;;; Parses a cube count, which should be of the form "n color"
(defun parse-cube-count (cube-set)
  (when (string-match " *\\([0-9]+\\) \\([a-z]*\\) *" cube-set)
    ;;; When matching a string, the match-string function requires you
    ;;; to give it the string you matched on, otherwise it tries to
    ;;; match against the current buffer.
    ;;; Also, read-from-string returns a tuple of the string and its position.
    ;;; We create a tuple here (a dotted pair, like "(blue . 5)"
    ;;; A list of these is called an association list and we can do
    ;;; lookups on it with assoc
    (cons (match-string 2 cube-set) (car (read-from-string (match-string 1 cube-set))))))

;;; Parses a game round, which is a series of cube counts separate by commas
(defun parse-round (game)
  (mapcar #'parse-cube-count (split-string game ",")))

;;; Parses a game, which has a game id, and then a series of rounds
;;; separated by semi-colons
(defun parse-game (line)
  (when (string-match "Game \\([0-9]+\\): \\(.*\\)" line)
    (cons (car (read-from-string (match-string 1 line)))
	  (mapcar #'parse-round (split-string (match-string 2 line) ";")))))

;;; Given a color name, a maximum value, and a round, see if the amount of
;;; that color in the round is less than the max allowable value
(defun is-valid-value (name maxval round)
  ;;; Look up the color in the round
  (let ((val (assoc name round)))
    ;;; Assoc returns the dotted pair or nil, use cdr to extract the
    ;;; right side of the pair if it is not nil
    (if val (<= (cdr val) maxval) t)))

(defun is-valid-round-part1 (round)
  (is-valid-round 12 13 14 round))

;;; Checks whether each color in a round is within the allowable limits
(defun is-valid-round (red green blue round)
  (and (is-valid-value "red" red round)
       (is-valid-value "green" green round)
       (is-valid-value "blue" blue round)))

;;; Checks whether all the rounds in the game are valid (i.e. every round is valid)
(defun is-valid-game (game)
  (seq-every-p #'is-valid-round-part1 (cdr game)))

;;; Add up the ids of each game
(defun valid-game-sum (games)
  (seq-reduce #'+
	      (mapcar #'car (seq-filter #'is-valid-game games))
	      0))

;;; Read the file, parse it, add up the valid games
(defun day2a ()
  (let* ((lines (read-lines "data/day2.txt"))
	 (games (mapcar #'parse-game lines)))
    (valid-game-sum games)))

;;; Returns the value of a color in a round, if the color isn't listed, return 0
(defun color-value (name round)
  (let ((val (assoc name round)))
    (if val (cdr val) 0)))

;;; Find the smallest value that is valid in all rounds, which is just the
;;; max value of that color
(defun smallest-color-in-round (name round)
  (apply 'max (mapcar (apply-partially #'color-value name) round)))

;;; Find the smallest initial configuration that will work in a game and
;;; multiple the red, green, and blue values together
(defun smallest-game (game)
  (* (smallest-color-in-round "red" (cdr game))
     (smallest-color-in-round "green" (cdr game))
     (smallest-color-in-round "blue" (cdr game))))

;;; Sum the smallest game values for each game
(defun smallest-game-sum (games)
  (seq-reduce '+ (mapcar #'smallest-game games) 0))

;;; Read the file, parse it, find the sum of the smallest initial values
(defun day2b ()
  (let* ((lines (read-lines "data/day2.txt"))
	  (games (mapcar #'parse-game lines)))
       (smallest-game-sum games)))

(message "Part 1: %d    Part 2: %d" (day2a) (day2b))

  
  
  
