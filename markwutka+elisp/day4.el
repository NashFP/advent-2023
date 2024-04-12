;;; This problem lends itself well to a more functional approach,
;;; so we can load in the file and convert it to a list of strings

;;; Load the file into a temporary buffer and convert it to a list of strings
(defun read-lines (file)
  (interactive)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((lines '()))
      (while (re-search-forward "\\(.*\\)[\n]" nil t)
	(setq lines (cons (match-string 1) lines)))
      (reverse lines))))

;;; Parse a card, skipping the card number, and creating a sort of
;;; dotted list with the winning numbers and the card numbers
(defun parse-card (line)
  (when (string-match ".*:\\([^|]*\\)[|] *\\(.*\\)" line)
    (cons
     ;;; Turn the parsed list of space-delimited numbers into a Lisp
     ;;; list and use the reader to turn it into a list of numbers
     (car (read-from-string (concat "(" (match-string 1 line) ")")))
     (car (read-from-string (concat "(" (match-string 2 line) ")"))))))

;;; Compute the score for the numbers on a card, doubling the score for
;;; each match beyond the first.
(defun score-numbers (winning numbers curr-score)
  (if (null numbers) curr-score
    (if (member (car numbers) winning)
	(if (= curr-score 0) (score-numbers winning (cdr numbers) 1)
	  (score-numbers winning (cdr numbers) (* curr-score 2)))
      (score-numbers winning (cdr numbers) curr-score))))

;;; Compute the score for a card using the recursive score-numbers routine
;;; with an initial score of 0
(defun score-card (card)
  (let ((winning (car card))
	(numbers (cdr card)))
    (score-numbers winning numbers 0)))    

;;; Load the file, parse it, sum the scores
(defun day4a ()
  (let* ((lines (read-lines "data/day4.txt"))
	 (cards (mapcar #'parse-card lines))
	 (scores (mapcar #'score-card cards)))
    (apply #'+ scores)))

;;; For part 2, we just need to know the number of winning numbers
(defun num-winners (card)
  (let ((winning (car card))
	(numbers (cdr card)))
    ;;; Return the length of the list of numbers containing
    ;;; only numbers that appear in winning
    (length (seq-filter (lambda (n) (member n winning)) numbers))))

;;; Create an initial count of the number of cards, initializing the
;;; count to 1. The indexes of the cards are numbered from 0
(defun make-card-count-table (score-vector)
  (let ((count-table (make-hash-table)))
    (dotimes (i (length score-vector))
      (puthash i 1 count-table))
    count-table))

;;; For a given card with n winning numbers increment the
;;; card count of the n next cards by the count of the
;;; given card.
(defun update-card-count (card-number card-counts score-vector)
  (let* ((card-score (elt score-vector card-number))
	 (card-count (gethash card-number card-counts)))
    ;;; For the card-scores cards after this one, increment their
    ;;; counts by this card's count
    (dotimes (n card-score)
      ;;; Make sure that card-number + n doesn't go off the end of the vector
      (when (< (+ card-number n 1) (length score-vector))
	;;; Update the card's count
	(puthash (+ card-number n 1) (+ (gethash (+ card-number n 1) card-counts) card-count)
		 card-counts)))))

;;; Starting at the first card, update the counts of each card in order
(defun update-card-counts (card-counts score-vector)
  (dotimes (n (length score-vector))
    (update-card-count n card-counts score-vector)))

;;; Sum the counts of all the cards
(defun sum-cards (card-counts score-vector)
  (let ((sum 0))
    (dotimes (n (length score-vector))
      (setq sum (+ sum (gethash n card-counts))))
    sum))

(defun day4b ()
  ;;; Load the file
  (let* ((lines (read-lines "data/day4.txt"))
	 ;;; Parse the cards
	 (cards (mapcar #'parse-card lines))
	 ;;; Create a scoring vector containing the number of winning numbers for each card
	 (score-vector (apply #'vector (mapcar #'num-winners cards)))
	 ;;; Create an initial card count table where there is 1 of each card
	 (card-counts (make-card-count-table score-vector)))
    ;;; Update the card counts based on the number of winning numbers
    (update-card-counts card-counts score-vector)
    ;;; Add up the total number of cards
    (sum-cards card-counts score-vector)))
						       
(message "Part 1: %d  Part 2: %d" (day4a) (day4b))
