(defun read-lines (file)
  (interactive)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((lines '()))
      (while (re-search-forward "\\(.*\\)[\n]" nil t)
	(setq lines (cons (match-string 1) lines)))
      (reverse lines))))

(defun split-groups (lines groups group)
  (cond
   ((null lines) (reverse (if (null group) groups (cons (reverse group) groups))))
   ((= (length (car lines)) 0) (split-groups (cdr lines) (cons (reverse group) groups) '()))
   (t (split-groups (cdr lines) groups (cons (car lines) group)))))

(defun parse-seeds (line)
  (cdr (car (read-from-string (concat "(" (car line) ")")))))

(defun parse-seed-line (line)
  (car (read-from-string (concat "(" line ")"))))

(defun compare-mapping (m1 m2)
  (< (cadr m1) (cadr m2)))

(defun parse-seed-map (group)
  (let* ((mappings (mapcar #'parse-seed-line (cdr group)))
	 (mappings-vec (apply #'vector mappings)))
    (sort mappings-vec #'compare-mapping)
    mappings-vec))

(defun find-offset (val start end mapping)
  (if (> start end) nil
    (let* ((mid (floor (+ start end) 2))
	   (mid-val (elt mapping mid)))
      (cond
       ((and (>= val (cadr mid-val))
	     (<= val (+ (cadr mid-val) (caddr mid-val))))
	mid)
       ((< val (cadr mid-val)) (find-offset val start (- mid 1) mapping))
       (t (find-offset val (+ mid 1) end mapping))))))
  
(defun find-entry1 (val start end mapping)
  (if (> start end) val
    (let* ((mid (floor (+ start end) 2))
	   (mid-val (elt mapping mid)))
      (cond
       ((and (>= val (cadr mid-val))
	     (<= val (+ (cadr mid-val) (caddr mid-val))))
	(+ (car mid-val) (- val (cadr mid-val))))
       ((< val (cadr mid-val)) (find-entry1 val start (- mid 1) mapping))
       (t (find-entry1 val (+ mid 1) end mapping))))))

(defun find-entry (val mapping)
  (let* ((pos (find-offset val 0 (- (length mapping) 1) mapping)))
    (if (null pos) val
      (let* ((pos-val (elt mapping pos))
	     (dest-start (car pos-val))
	     (from-start (cadr pos-val))
	     (from-length (caddr pos-val)))
	(+ dest-start (+ (- val from-start)))))))

(defun apply-mappings (val mappings)
  (if (null mappings) val
    (apply-mappings (find-entry val (car mappings)) (cdr mappings))))

(defun day5a ()
  (let* ((lines (read-lines "data/day5.txt"))
	 (groups (split-groups lines '() '()))
	 (seeds (parse-seeds (car groups)))
	 (seed-to-soil (parse-seed-map (elt groups 1)))
	 (soil-to-fertilizer (parse-seed-map (elt groups 2)))
	 (fertilizer-to-water (parse-seed-map (elt groups 3)))
	 (water-to-light (parse-seed-map (elt groups 4)))
	 (light-to-temperature (parse-seed-map (elt groups 5)))
	 (temperature-to-humidity (parse-seed-map (elt groups 6)))
	 (humidity-to-location (parse-seed-map (elt groups 7)))
	 (mapping-chain (list seed-to-soil soil-to-fertilizer
			      fertilizer-to-water water-to-light
			      light-to-temperature temperature-to-humidity
			      humidity-to-location)))
    (apply #'min (mapcar (lambda (val) (apply-mappings val mapping-chain)) seeds))))

(message "Part 1: %d" (day5a))

