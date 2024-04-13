(defun split-groups (lines groups group)
  (cond
   ((null lines) (reverse (if (null group) groups (cons (reverse group) groups))))
   ((= (length (car lines)) 0) (split-groups (cdr lines) (cons (reverse group) groups) '()))
   (t (split-groups (cdr lines) groups (cons (car lines) group)))))
