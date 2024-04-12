;;; The strategy here is to take the buffer and edit it
;;; into a Lisp list that we can then read from the buffer
;;; and process.

;;; Create some regular expressions for parsing the values
(setq nondigit '("[^0-9\r\n]+" . ""))
(setq duplicate-single '("^\\([0-9]\\)$" . "\\1\\1"))
(setq firstlast '("\\([0-9]\\)[0-9]*\\([0-9]\\)" . "\\1\\2"))
(setq numbers '(("one" . "1")
		("two" . "2")
		("three" . "3")
		("four" . "4")
		("five" . "5")
		("six" . "6")
		("seven" . "7")
		("eight" . "8")
		("nine" . "9")))

;;; Repeatedly applies a pattern to the entire buffer starting at the beginning
(defun do-replace (pattern)
  ;;; Go to the beginning
  (goto-char (point-min))
  ;;; Look for the regex
  (while (re-search-forward (car pattern) nil t)
    ;;; Replace it with the replacement pattern
    (replace-match (cdr pattern))))

;;; We have to be more careful when replacing digit strings because
;;; the example file may do tricking things like eightwo where if you
;;; replaced "two" first it would be wrong. So instead we take a very
;;; conservative approach and try a substitution at each position
(defun replace-digits ()
  ;;; While we aren't at the end of the file
  (while (not (eq (point) (point-max)))
    ;;; Try each number pattern
    (dolist (pattern numbers)
      ;;; Remember where we are in the file
      (let ((pt (point)))
	;;; Is this pattern present starting at the current point
	(when (looking-at (car pattern))
	  ;;; If so, replace it
	  (replace-match (cdr pattern)))
	;;; Go back to the last position
	(goto-char pt)))
    ;;; Once we have tried each match, move forward one char and
    ;;; try all the matches again
    (forward-char)))
    
;;; I often have dayNa and dayNb functions for each part of a
;;; problem, but each part here is almost identical except
;;; that in part two you have to look for numbers as words,
;;; which we do here when interpret-numbers is true
(defun day1 (interpret-numbers)
  (interactive)
  ;;; Create a temporary buffer
  (with-temp-buffer
    ;;; Load the problem data file
    (insert-file-contents "data/day1.txt")

    ;;; If we are supposed to interpret string names of numbers, do that
    (when interpret-numbers
      (replace-digits))
    
    ;;; Remove everything that isn't a digit
    (do-replace nondigit)
    
    ;;; If a line has a single digit, duplicate that digit
    (do-replace duplicate-single)

    ;;; Remove all the digitis between the first and the last
    (do-replace firstlast)

    ;;; Go to the beginning of the file and insert a (
    (goto-char (point-min))
    (insert-char ?\()

    ;;; Go to the end of the file and insert a )
    (goto-char (point-max))
    (insert-char ?\))

    ;;; Go back to the beginning
    (goto-char (point-min))

    ;;; Read the current buffer as a lisp list
    (let ((numbers (read (current-buffer))))
      ;;; Sum the numbers
      (seq-reduce #'+ numbers 0))))

;;; Run the function twice, once with interpret-numbers false (nil) and once true
(message "Part 1: %d   Part 2: %d" (day1 nil) (day1 t))
  
  
