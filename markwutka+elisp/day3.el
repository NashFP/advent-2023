;;; This problem requires some 2-dimensional reasoning, so
;;; we'll just load the file into a temporary buffer and
;;; navigate around the buffer.

;;; Compute the length of the first line (which includes a newline)
;;; This is used to move up and down lines since the current point is
;;; just the offset from the beginning
(defun compute-line-length ()
  (when (re-search-forward "\n" nil t)
    (match-beginning 0)))

;;; Returns true if a character is not a newline a . or a digit
(defun is-adj-char (ch)
  (not (or (= ch ?\n)
	   (= ch ?\.)
	   (and (>= ch ?0) (<= ch ?9)))))

;;; Returns true if a part symbol occurs in the given range in the file
(defun has-char (start end)
  ;;; If start is before the beginning of the file, just move to the
  ;;; right and check again
  (if (< start 1) (has-char (+ start 1) end)
  ;;; Otherwise, If start is after end, or at the end of the file, return nil
    (if (or (> start end) (>= start (point-max))) nil
    ;;; Otherwise, go to start and see if this is a part symbol
      (progn
	(goto-char start)
        ;;; If so, return true
	(if (is-adj-char (char-after start)) t
	  ;;; Otherwise, move to the right on char and check again
	  (has-char (+ start 1) end))))))

;;; Checks to see if a range in the file borders on one of the part
;;; symbols.
(defun has-adjacent (start end line-length)
  (let ((start-point (point))
        (result
	 ;;; Check the previous line, starting one position to the
	 ;;; left of start, and extending one to the right of end
	 (or (has-char (- start (+ line-length 1)) (- end line-length))
	     ;;; Check the character right before start
	     (has-char (- start 1) (- start 1))
	     ;;; Check the character after the end
	     (has-char end end)
	     ;;; Check the next line, starting one to the left of start
	     ;;; and going to one to the right of end
	     (has-char (+ start (- line-length 1)) (+ end line-length)))))
    (goto-char start-point)
    result))

;;; Find all the numbers that are adjacent to a part symbol
;;; This is again a little less functional in that it is using setq locally
(defun find-valid-numbers (line-length)
  (goto-char (point-min))
  (let ((result '()))
    (while (re-search-forward "[0-9]+" nil t)
      ;;; If this number is adjacent to a part symbol, add it to the list
      (when (has-adjacent (match-beginning 0) (match-end 0) line-length)
	;;; Read the part number from the matched string and add it to the result list
	(setq result (cons (car (read-from-string (match-string 0))) result))))
    ;;; Since we just sum these, there's technically no reason to reverse the
    ;;; list, but it might help with debugging if there is a problem
    (seq-reverse result)))

(defun day3a ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents "data/day3.txt")
    (let* ((line-length (compute-line-length))
	   (valid-numbers (find-valid-numbers line-length)))
      (apply #'+ valid-numbers))))

;;; For part 2, we still use the buffer in place, but since we only care about
;;; the parts that are gears, we remove all the parts that aren't gears. Then,
;;; rather than eliminating numbers that aren't adjacent to a part, when we look
;;; at all the spaces surrounding a number, and create a table, such that for
;;; every unique space where there is a gear, we add any part numbers that are
;;; adjactent to it to a list in the table.

;;; Replace anything that isn't newline, a digit, a . or a gear with a .
(defun replace-non-gears ()
  (while (re-search-forward "[^\n0-9.*]" nil t)
    (replace-match "."))
  (goto-char (point-min)))

;;; See if there is a gear in the range from start to end, and if
;;; there is, add the part number to the entry in gear-table keyed
;;; by the gear's position
(defun find-gear (start end num gear-table)
  ;;; If start is before the first position, move it to the right
  ;;; and try again
  (if (< start 1) (find-gear (+ start 1) end num gear-table)
    ;;; Otherwise, if start is not past end, or past the end of file,
    ;;; examine the character at start
    (when (not (or (> start end) (>= start (point-max))))
      (goto-char start)
      ;;; If it is a gear...
      (when (= (char-after start) ?\*)
	;;; Update the hash table for this position, adding the part number to
	;;; its list of part numbers
	(puthash start (cons num (gethash start gear-table '())) gear-table))
      ;;; Now try the position to the right of start
      (find-gear (+ start 1) end num gear-table))))

;;; Given a number that exists between positions start and end, look at all
;;; the spots adjacent to that range, and if any contain a *, add the number
;;; to that gear's list in the table
(defun find-gears (start end line-length num gear-table)
  (let ((start-point (point)))
    (find-gear (- start (+ line-length 1)) (- end line-length) num gear-table)
    (find-gear (- start 1) (- start 1) num gear-table)
    (find-gear end end num gear-table)
    (find-gear (+ start (- line-length 1)) (+ end line-length) num gear-table)
    (goto-char start-point)))

;;; Compute the gear ratio by multiplying all the numbers for that gear together
;;; if there are 2 or more, otherwise return 0
(defun gear-ratio (numbers)
  (if (< (length numbers) 2) 0
    (apply #'* numbers)))

;;; Sum all the gear ratios for the gears in the table
(defun add-gears (gear-table)
  (let ((result '()))
    ;;; maphash is not as functionally friendly as we would like, it always
    ;;; returns nil, so we have to use setq and update the return value locally
    (maphash (lambda (gear numbers)
	       (setq result (cons (gear-ratio numbers) result)))
	     gear-table)
    (apply #'+ result)))

;;; Find all the numbers in the file and look for gears around them, then
;;; sum their gear ratios
(defun find-gear-numbers (line-length)
  (goto-char (point-min))
  (let ((gear-table (make-hash-table)))
    (while (re-search-forward "[0-9]+" nil t)
      (let ((num (car (read-from-string (match-string 0)))))
	(find-gears (match-beginning 0) (match-end 0) line-length num gear-table)))
    (add-gears gear-table)))

(defun day3b ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents "data/day3.txt")
    (replace-non-gears)
    (let ((line-length (compute-line-length)))
      (find-gear-numbers line-length))))
    
(message "Part 1: %d   Part 2: %s" (day3a) (day3b))
